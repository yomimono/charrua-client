let random_mac () =
  Macaddr.make_local @@ function
    | 0 -> ((Random.int 63) lsl 2)
    | _ -> (Random.int 255)

let read_packet ic =
  let fixed_dhcp_header = 4*7+16+64+128 in
  let minsize = Ethif_wire.sizeof_ethernet
                + Ipv4_wire.sizeof_ipv4
                + Udp_wire.sizeof_udp
                + fixed_dhcp_header in
  let buf = Bytes.create minsize in
  try
    let add_some base = function
      | n when n <= 0 -> Ok base
      | extra ->
        let extra_payload = Bytes.create extra in
        really_input ic extra_payload 0 extra;
        Ok (Bytes.cat base extra_payload)
    in
    really_input ic buf 0 minsize;
    let ipv4 = Cstruct.of_bytes
      (Bytes.sub buf Ethif_wire.sizeof_ethernet Ipv4_wire.sizeof_ipv4) in
    let ipv4_hlen = ((Ipv4_wire.get_ipv4_hlen_version ipv4) lsr 4) * 4 in
    let ipv4_payload_len = Ipv4_wire.get_ipv4_len ipv4 in
    let ipv4_adjustment = max 0 (ipv4_hlen - Ipv4_wire.sizeof_ipv4) in
    let more_to_read = (ipv4_payload_len
                       - ipv4_adjustment
                       - Udp_wire.sizeof_udp
                       - fixed_dhcp_header) in
    add_some buf more_to_read
  with | Invalid_argument _ -> Error "too small"

let try_lease ?(mac=random_mac ()) ic oc =
  let (client, buf) = Dhcp_client.create mac in
  let () = output_string oc (Cstruct.to_string buf) in
  let () = flush oc in
  let rec step client =
    match Dhcp_client.lease client with
    | Some lease -> (client, lease)
    | None ->
      match read_packet ic with
      | Error e ->
        Printf.eprintf "%s: %s\n%!" "You'll have to try harder than that to make a readable packet!!!" e; step client
      | Ok input_buffer ->
        match Dhcp_client.input client (Cstruct.of_bytes input_buffer) with
        | `Noop ->
          Format.eprintf "%s\n%!" "You'll have to try harder than that to advance the state machine!!!";
          step client
        | `New_lease l -> l
        | `Response (client, action) ->
          let () = output_string oc (Cstruct.to_string action) in
          flush oc;
          step client
  in
  step client

(* try_renew will try ad inifinitum to renew an existing lease;
 * it needs external controls to recognize that it has entered the REBINDING
 * period and should give up and begin a new lease transaction *)
let try_renew ic oc client =
  match Dhcp_client.renew client with
  | `Noop -> None
  | `Response (client, buf) ->
    let () = output oc (Bytes.of_string @@ Cstruct.to_string buf) 0 (Cstruct.len buf) in
    let rec step client =
      match read_packet ic with
      | Error e ->
        Printf.eprintf "%s: %s\n%!" "You'll have to try harder than that to make a readable packet!!!" e; step client
      | Ok input_buffer ->
        match Dhcp_client.input client (Cstruct.of_bytes input_buffer) with
        | `Noop -> (* wait for useful input *) step client
        | `New_lease l -> Some l
        | `Response (client, action) ->
          output oc (Bytes.of_string @@ Cstruct.to_string action) 0 (Cstruct.len action);
          flush stdout;
          step client
    in
    step client

module Make(Time : Mirage_time_lwt.S) = struct
  
  let rec get_lease ?(timeout=Duration.of_sec 3) ~mac ic oc : (Dhcp_client.t * Dhcp_wire.pkt) Lwt.t =
    let retry timeout = Lwt.bind (Time.sleep_ns timeout) (fun () ->
        Printf.eprintf "Timeout %f expired with no lease; starting over" @@ Duration.to_f timeout;
        get_lease ~timeout ~mac ic oc
      )
    in
    Lwt.pick [
      Lwt.return @@ try_lease ~mac ic oc;
      retry timeout;
    ]
 
  (* [rebinding_time] is the timeout until we should give up renewing the lease,
   * and just try to get a new lease entirely (i.e., enter the REBINDING state).
   * [retry_time] is the timeout between successive lease renewal attempts.
   * If [retry_time] is longer than [rebinding_time], only one attempt to renew
   * the lease will be made before attempting to get an entirely new lease.
   * *)
  let get_renew ?(rebinding_time=Duration.of_sec 2) ?(retry_time=Duration.of_sec 1) ~mac ic oc client =
    let rec renew () =
      Lwt.pick [
        Lwt.return @@ try_renew ic oc client;
        Lwt.bind (Time.sleep_ns retry_time) (fun () ->
          Printf.eprintf "Timeout %f expired with no renewal; trying again" @@ Duration.to_f retry_time;
          renew ())
      ]
    in
    Lwt.pick [
      renew ();
      Lwt.bind (Time.sleep_ns rebinding_time) (fun () ->
        Printf.eprintf "Timeout %f expired with no renewal; abandoning this lease and trying for a new one" @@ Duration.to_f rebinding_time;
        Lwt.map (fun l -> Some l) (get_lease ~mac ic oc)
      )
    ]
  
end

let get_times options =
  let t2_timeout = match Dhcp_wire.find_rebinding_t2 options with
  | Some time -> Duration.of_sec @@ Int32.to_int time | None -> Duration.of_sec 3600
  in
  let t1_timeout = match Dhcp_wire.find_renewal_t1 options with
  | Some time -> Duration.of_sec @@ Int32.to_int time | None -> Int64.div t2_timeout 2L
  in
  let delay_time = match Int64.compare t1_timeout t2_timeout with
  | n when n < 0 -> Int64.sub t2_timeout t1_timeout
  | _ -> t1_timeout
  in
  (t1_timeout, delay_time)

module Time =
  struct type 'a io = 'a Lwt.t let sleep_ns a = Lwt_unix.sleep (Duration.to_f a) end

(* a pretty bad text adventure (now with waiting!) *)
let () =
  let open Lwt.Infix in
  let module Client = Make(Time) in
  set_binary_mode_in stdin true;
  set_binary_mode_out stdout true;
  let mac = random_mac () in
  Lwt_main.run @@
    Lwt.catch (fun () ->
      Client.get_lease ~mac ~timeout:(Duration.of_ms 1) stdin stdout >>= fun (client, lease) ->
      Printf.eprintf "You win!: %s\n%!" @@ Dhcp_wire.pkt_to_string lease;
      Printf.eprintf "%s\n%!" "Get ready for bonus round!!!";
      let rec renew client mac lease =
        let (t1_timeout, delay_time) = get_times lease.Dhcp_wire.options in
        Time.sleep_ns t1_timeout >>= fun () ->
        Client.get_renew ~mac ~rebinding_time:delay_time stdin stdout client >>= function
        | Some (c, new_lease) ->
          Printf.eprintf "%s\n%!" "BONUS ROUND WIN!!!\n%!";
          Printf.eprintf "%s\n%!" @@ Dhcp_wire.pkt_to_string new_lease;
          renew c mac new_lease
        | None ->
          Printf.eprintf "%s\n%!" "you lost the bonus round :(";
          exit 1
      in
      renew client mac lease
    )
    (
      function | End_of_file -> exit 1
               | exn -> raise exn
    )
