let random_mac () =
  Macaddr.make_local @@ function
    | 0 -> ((Random.int 63) lsl 2)
    | _ -> (Random.int 255)

module Time =
  struct type 'a io = 'a Lwt.t let sleep_ns a = Lwt_unix.sleep (Duration.to_f a) end

module Client = Dhcp_client_lwt.Make(Time)(Netif_fd)

(* a pretty bad text adventure (now with waiting!) *)
let () =
  let open Lwt.Infix in
  Lwt_main.run @@ (
    Netif_fd.connect ~mac:(random_mac ()) Unix.stdin >>= fun net ->
    Client.connect net >>= fun leases ->
    let rec print_lease leases =
      Lwt.catch (fun () ->
        Lwt_stream.last_new leases >>= fun lease ->
        (Format.printf "got a lease!!! %s\n" @@ Dhcp_wire.pkt_to_string lease;
        print_lease leases)
      )
      (function Lwt_stream.Empty -> Lwt.return_unit)
    in
    print_lease leases
  )
