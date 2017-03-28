module Time =
  struct type 'a io = 'a Lwt.t let sleep_ns a = Lwt_unix.sleep (Duration.to_f a) end

module Half_netif = struct
  include Netif_fd
  let print buf = Format.printf "%a\n%!" Cstruct.hexdump_pp buf

  let write _ buf =
    print buf; Lwt.return (Ok ())

  let writev _ bufs =
    List.iter print bufs; Lwt.return (Ok ())
end

module Client = Dhcp_client_lwt.Make(Time)(Half_netif)

let () =
  let mac = Macaddr.of_string_exn "c0:ff:33:c0:ff:ee" in
  let xid = 0xabad1deal in
  let open Lwt.Infix in
  Lwt_main.run @@ (
    Half_netif.connect ~mac Unix.stdin >>= fun net ->
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
