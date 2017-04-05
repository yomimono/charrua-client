let config_of_lease lease : (int32 * int32 * Mirage_protocols_lwt.ipv4_config) option =
  let open Dhcp_wire in
  let expiration lease =
    let t1 = match Dhcp_wire.find_renewal_t1 lease with
    | Some n -> n | None -> 1800l (* 30 minutes *)
    in
    let t2 = match Dhcp_wire.find_rebinding_t2 lease with
    | Some n -> n | None -> 3600l (* 1 hour *)
    in
    (t1, t2)
  in
  (* ipv4_config expects a single IP address and the information
   * needed to construct a prefix.  It can optionally use one router. *)
  let address = lease.yiaddr in
  match Dhcp_wire.find_subnet_mask lease.options with
  | None -> None
  | Some subnet ->
    let t1, t2 = expiration lease.options in
    let network = Ipaddr.V4.Prefix.of_netmask subnet address in
    let valid_routers = Dhcp_wire.collect_routers lease.options in
    match valid_routers with
    | [] -> Some (t1, t2, Mirage_protocols_lwt.{ address; network; gateway = None })
    | hd::_ ->
      Some (t1, t2, Mirage_protocols_lwt.{ address; network; gateway = (Some hd) })

let src = Logs.Src.create "dhcp_client"
module Log = (val Logs.src_log src : Logs.LOG)

module Make(Time : Mirage_types_lwt.TIME) (Net : Mirage_types_lwt.NETWORK) = struct
  open Lwt.Infix
  open Mirage_protocols_lwt

  type t = ipv4_config Lwt_stream.t

  let usable_config_of_lease = function
  | None -> None
  | Some lease -> config_of_lease lease

  let connect ?(requests : Dhcp_wire.option_code list option) net =
    (* listener needs to occasionally check to see whether the state has advanced,
     * and if not, start a new attempt at a lease transaction *)
    let sleep_interval = Duration.of_sec 5 in

    let (client, dhcpdiscover) = Dhcp_client.create ?requests (Net.mac net) in
    let c = ref client in

    (* if the lease specifies an expiration time, sleep that long and then
     * try renewing.
     * Note that this will only *send* packets, and in the context of a
     * Mirage application which has already started with the lease configuration,
     * attempting to spy on the listen loop to hear any replies would
     * disrupt the user application (as calls to `Net.listen` overwrite any
     * previous listener).  Therefore it's not currently possible to hear
     * any reply to the request for renewal, be it positive or negative. *)

    (* this logic is also not quite right for the case where we can
     * do something about a failure to bind -- what we should actually do is
     * also check the t2 timer and then start a new full lease transaction,
     * but that's not possible for us for the reasons above. *)
    let rec renew c t =
      Time.sleep_ns @@ Duration.of_sec t >>= fun () ->
      match Dhcp_client.renew c with
      | None -> Log.debug (fun f -> f "Don't need to renew the lease; won't try");  Lwt.return_unit
      | Some (c, buf) ->
        Log.debug (fun f -> f "attempted to renew lease: %a" Dhcp_client.pp c);
        Net.write net buf >|= Rresult.R.get_ok >>= fun () ->
        renew c t (* ideally t would come from the new lease... *)
    in
    let rec get_lease dhcpdiscover =
      Log.debug (fun f -> f "Sending DHCPDISCOVER...");
      Net.write net dhcpdiscover >|= Rresult.R.get_ok >>= fun () ->
      Time.sleep_ns sleep_interval >>= fun () ->
      match usable_config_of_lease (Dhcp_client.lease !c) with
      | Some (t1, _t2, config) ->
        Log.info (fun f -> f "Lease obtained! Renewal will begin in %lu seconds. IP: %a, network: %a, gateway: %a"
          t1 Ipaddr.V4.pp_hum config.address Ipaddr.V4.Prefix.pp_hum config.network
          (Fmt.option Ipaddr.V4.pp_hum) config.gateway);
          (* to keep the lease valid, send renewal messages *)
          Lwt.async (fun () -> renew !c @@ Int32.to_int t1);
          Lwt.return (Some config)
      | None ->
        let (client, dhcpdiscover) = Dhcp_client.create ?requests (Net.mac net) in
        c := client;
        Log.info (fun f -> f "Timeout expired without a usable lease!  Starting over...");
        Log.debug (fun f -> f "New lease attempt: %a" Dhcp_client.pp !c);
        get_lease dhcpdiscover
    in
    let listen () =
      Net.listen net (fun buf ->
        match Dhcp_client.input !c buf with
        | (s, Some action) -> Net.write net action >|=
          Rresult.R.get_ok >>= fun () ->
          Log.debug (fun f -> f "State advanced! Now %a" Dhcp_client.pp s);
          c := s; Lwt.return_unit
        | (s, None) ->
          Log.debug (fun f -> f "No action! State is %a" Dhcp_client.pp s);
          c := s; Lwt.return_unit
      ) >|= Rresult.R.get_ok
    in
    let lease_wrapper () =
      Lwt.pick [
        (listen () >>= fun () -> Lwt.return None); (* if canceled, the stream should end *)
        get_lease dhcpdiscover; (* will terminate once a lease is obtained *)
      ]
    in
    let s = Lwt_stream.from lease_wrapper in
    Lwt.return s

end
