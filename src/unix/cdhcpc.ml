module Time =
  struct type 'a io = 'a Lwt.t let sleep_ns a = Lwt_unix.sleep (Duration.to_f a) end

module Client = Dhcp_client_lwt.Make(Time)(Netif_fd)

let () =
  let mac = Macaddr.of_string_exn "c0:ff:33:c0:ff:ee" in
  let xid = 0xabad1deal in
  let open Lwt.Infix in
  Lwt_main.run @@ (
    Netif_fd.connect ~mac Unix.stdin >>= fun net ->
    Client.connect ~with_xid:xid net >>= fun leases ->
    let rec use_lease leases =
      Lwt.catch (fun () ->
        Lwt_stream.last_new leases >>= fun lease ->
        Priv.set_config lease >>= fun () ->
        use_lease leases
      )
      (function Lwt_stream.Empty -> Lwt.return_unit)
    in
    use_lease leases
  )
