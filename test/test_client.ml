let cstruct = Alcotest.of_pp Cstruct.hexdump_pp

let msgtype =
  let module M = struct
    type t = Dhcp_wire.msgtype
    let pp fmt m = Format.fprintf fmt "%s" (Dhcp_wire.msgtype_to_string m)
    let equal p q = (compare p q) = 0
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let empty_db = Dhcp_server.Lease.make_db ()

module Defaults = struct
  let client_mac = Macaddr.of_string_exn "00:16:3e:ba:eb:ba"
  let server_mac = Macaddr.of_string_exn "00:16:3e:00:00:00"
  let server_ip = Ipaddr.V4.of_string_exn "192.168.1.1"
  let server_network = Ipaddr.V4.Prefix.make 24 server_ip
  let range = Some (Ipaddr.V4.of_string_exn "192.168.1.15", Ipaddr.V4.of_string_exn "192.168.1.65")
  let options = []
  let config = Dhcp_server.Config.make
    ?hostname:None ?default_lease_time:None
    ?max_lease_time:None ?hosts:None
    ~addr_tuple:(server_ip, server_mac)
    ~network:server_network ~range ~options
end

let parseable buf = 
  Alcotest.(check bool) "buffer we constructed is valid dhcp" true (Dhcp_wire.is_dhcp buf (Cstruct.len buf))

let assert_reply p =
  let open Dhcp_server.Input in
  match p with
  | Warning s | Error s -> Alcotest.fail s
  | Silence -> Alcotest.fail "Silence from the server in response to a request"
  | Update _db -> Alcotest.fail "database update but no reply -- in our context this is likely a bug"
  | Reply (pkt, db) -> (pkt, db)

let start_makes_dhcp () =
  let (_s, buf) = Dhcp_client.create Defaults.client_mac in
  (* for now, any positive result is fine *)
  parseable buf

let client_to_selecting () =
  let open Defaults in
  let (s, buf) = Dhcp_client.create client_mac in
  let answer = Dhcp_wire.pkt_of_buf buf (Cstruct.len buf) in
  Alcotest.(check (result pass reject)) "input succeeds" answer answer;
  (s, Rresult.R.get_ok answer)

let client_to_bound () =
  let open Dhcp_wire in
  let open Defaults in
  let (s, dhcpdiscover) = client_to_selecting () in
  let (dhcpoffer, db) = assert_reply @@ Dhcp_server.Input.input_pkt config empty_db dhcpdiscover 0l in
  match Dhcp_client.input s (Dhcp_wire.buf_of_pkt dhcpoffer) with
  | `Noop -> Alcotest.fail "couldn't get client to respond to DHCPOFFER"
  | `New_lease _ -> Alcotest.fail "got a lease in response to DHCPOFFER"
  | `Response (s, buf) ->
    let dhcprequest = Rresult.R.get_ok @@ Dhcp_wire.pkt_of_buf buf (Cstruct.len buf) in
    let (dhcpack, _db) = assert_reply @@ Dhcp_server.Input.input_pkt config db dhcprequest 0l in
    Alcotest.(check (option msgtype)) "got a DHCPACK in response to DHCPREQUEST"
      (Some DHCPACK) (find_message_type dhcpack.options);
    match Dhcp_client.input s (Dhcp_wire.buf_of_pkt dhcpack) with
    | `Response _ ->
       Alcotest.fail "client wanted to send more packets after receiving DHCPACK"
    | `Noop ->
       Alcotest.fail "client didn't realize it got a lease"
    | `New_lease (s, _lease) ->
       match Dhcp_client.lease s with
       | None -> Alcotest.fail "Dhcp_client.lease failed on a client in BOUND state"
       | Some lease -> db, s, lease

let server_accepts_start_packet () =
  let open Defaults in
  let (_, dhcpdiscover) = client_to_selecting () in
  ignore @@ assert_reply @@ Dhcp_server.Input.input_pkt config empty_db dhcpdiscover 0l

let server_gives_dhcpoffer () =
  let open Defaults in
  let open Dhcp_wire in
  let (_, dhcpdiscover) = client_to_selecting () in
  let (pkt, _db) = assert_reply @@
    Dhcp_server.Input.input_pkt config empty_db dhcpdiscover 0l in
  Alcotest.(check (option msgtype)) "initial message merited a DHCPOFFER"
    (Some DHCPOFFER) (find_message_type pkt.options)

let client_rejects_wrong_xid () =
  let open Defaults in
  let (s, answer) = client_to_selecting () in
  let (pkt, _db) = assert_reply @@ Dhcp_server.Input.input_pkt config empty_db answer 0l in
  let pkt = Dhcp_wire.({pkt with xid = Int32.add pkt.xid 1l}) in
  match Dhcp_client.input s @@ Dhcp_wire.buf_of_pkt pkt with
  | `Response _ | `New_lease _ -> Alcotest.fail "responded to dhcpoffer with wrong xid"
  | `Noop -> ()

let client_asks_dhcprequest () = begin
  [@ocaml.warning "-4"]
  let open Dhcp_wire in
  let open Defaults in
  let (s, answer) = client_to_selecting () in
  let (pkt, _db) = assert_reply @@ Dhcp_server.Input.input_pkt config empty_db answer 0l in
  match find_message_type pkt.options with
  | Some DHCPOFFER -> begin
    match Dhcp_client.input s (Dhcp_wire.buf_of_pkt pkt) with
    | `Noop -> Alcotest.fail "response to DHCPOFFER was silence"
    | `New_lease _ -> Alcotest.fail "thought a DHCPOFFER was a lease"
    | `Response (_s, buf) ->
      parseable buf;
      let dhcprequest = Rresult.R.get_ok @@ Dhcp_wire.pkt_of_buf buf (Cstruct.len buf) in
      Alcotest.(check (option msgtype)) "responded to DHCPOFFER with DHCPREQUEST"
        (Some DHCPREQUEST) (find_message_type dhcprequest.options)
  end
  | _ -> Alcotest.fail "couldn't get a valid DHCPOFFER to attempt to send DHCPREQUEST in response to"
end

let server_gives_dhcpack () =
  let open Dhcp_wire in
  let open Defaults in
  let (s, dhcpdiscover) = client_to_selecting () in
  let (dhcpoffer, db) = assert_reply @@ Dhcp_server.Input.input_pkt config empty_db dhcpdiscover 0l in
  match Dhcp_client.input s (Dhcp_wire.buf_of_pkt dhcpoffer) with
  | `Noop -> Alcotest.fail "couldn't get client to respond to DHCPOFFER"
  | `New_lease _ -> Alcotest.fail "thought a DHCPOFFER was a valid lease"
  | `Response (_s, buf) ->
    let dhcprequest = Rresult.R.get_ok @@ Dhcp_wire.pkt_of_buf buf (Cstruct.len buf) in
    let (dhcpack, _db) = assert_reply @@ Dhcp_server.Input.input_pkt config db dhcprequest 0l in
      Alcotest.(check (option msgtype)) "got a DHCPACK in response to DHCPREQUEST"
        (Some DHCPACK) (find_message_type dhcpack.options)

let client_returns_lease () =
  let open Dhcp_wire in
  let open Defaults in
  let (s, dhcpdiscover) = client_to_selecting () in
  let (dhcpoffer, db) = assert_reply @@ Dhcp_server.Input.input_pkt config empty_db dhcpdiscover 0l in
  match Dhcp_client.input s (Dhcp_wire.buf_of_pkt dhcpoffer) with
  | `Noop -> Alcotest.fail "couldn't get client to respond to DHCPOFFER"
  | `New_lease _ -> Alcotest.fail "thought a DHCPOFFER was a valid lease"
  | `Response (s, buf) ->
    let dhcprequest = Rresult.R.get_ok @@ Dhcp_wire.pkt_of_buf buf (Cstruct.len buf) in
    let (dhcpack, _db) = assert_reply @@ Dhcp_server.Input.input_pkt config db dhcprequest 0l in
    Alcotest.(check (option msgtype)) "got a DHCPACK in response to DHCPREQUEST"
      (Some DHCPACK) (find_message_type dhcpack.options);
    match Dhcp_client.input s (Dhcp_wire.buf_of_pkt dhcpack) with
    | `Response _ ->
       Alcotest.fail "client wanted to send more packets after receiving DHCPACK"
    | `Noop -> Alcotest.fail "client refused a nice lease"
    | `New_lease (s, _lease) ->
       Alcotest.(check (option pass)) "lease is held" (Some dhcpack) (Dhcp_client.lease s)

let no_lease_no_renewal () =
  let (s, _discover) = client_to_selecting () in
  match Dhcp_client.renew s with
  | `Response _ -> Alcotest.fail "renewal action given for non-bound client"
  | `Noop -> ()

let renew_generates_packet () =
  let open Dhcp_wire in
  let (_db, s, _lease) = client_to_bound () in
  match Dhcp_client.renew s with
  | `Noop -> Alcotest.fail "renew refused to cooperate with reasonable state"
  | `Response (_s, renewal) ->
  let renewal = Rresult.R.get_ok @@ Dhcp_wire.pkt_of_buf renewal (Cstruct.len renewal) in
  Alcotest.(check (option msgtype)) "send dhcprequest when renewing" (Some DHCPREQUEST) (find_message_type renewal.options)

let server_accepts_renewal () =
  let open Dhcp_wire in
  let db, s, _lease = client_to_bound () in
  match Dhcp_client.renew s with
  | `Noop -> Alcotest.fail "renew refused to cooperate with reasonable state"
  | `Response (_s, renewal) ->
  let renewal = Rresult.R.get_ok @@ Dhcp_wire.pkt_of_buf renewal (Cstruct.len renewal) in
  let (dhcpack, _db) = assert_reply @@ Dhcp_server.Input.input_pkt Defaults.config db renewal 0l in
  Alcotest.(check (option msgtype)) "got a DHCPACK in response to DHCPREQUEST for renewal"
    (Some DHCPACK) (find_message_type dhcpack.options)

let client_accepts_renewal () =
  let open Dhcp_wire in
  let db, s, _lease = client_to_bound () in
  match Dhcp_client.renew s with
  | `Noop -> Alcotest.fail "renew refused to cooperate with reasonable state"
  | `Response (s, renewal) ->
  let renewal = Rresult.R.get_ok @@ Dhcp_wire.pkt_of_buf renewal (Cstruct.len renewal) in
  let (dhcpack, _db) = assert_reply @@ Dhcp_server.Input.input_pkt Defaults.config db renewal 0l in
  Alcotest.(check (option msgtype)) "got a DHCPACK in response to DHCPREQUEST for renewal"
    (Some DHCPACK) (find_message_type dhcpack.options);
  match Dhcp_client.input s (Dhcp_wire.buf_of_pkt dhcpack) with
  | `Noop -> Alcotest.fail "client refused a requested renewal"
  | `Response _ ->
     Alcotest.fail "client wanted to send more packets after receiving DHCPACK in renewal"
  | `New_lease (s, _) ->
     Alcotest.(check (option pass)) "lease is held" (Some dhcpack) (Dhcp_client.lease s)

let client_rebinds () =
  let open Dhcp_wire in
  let _db, s, _lease = client_to_bound () in
  match Dhcp_client.renew s with
  | `Noop -> Alcotest.fail "renew refused to cooperate with reasonable state"
  | `Response (s, renewal) ->
  let renewal = Rresult.R.get_ok @@ Dhcp_wire.pkt_of_buf renewal (Cstruct.len renewal) in
  let leaseless_server = Dhcp_server.Config.{ Defaults.config with range = None } in
  let (dhcpnak, _db) = assert_reply @@ Dhcp_server.Input.input_pkt leaseless_server empty_db renewal 0l in
  Alcotest.(check (option msgtype)) "got a DHCPNAK in response to DHCPREQUEST for renewal"
    (Some DHCPNAK) (find_message_type dhcpnak.options);
  match Dhcp_client.input s (Dhcp_wire.buf_of_pkt dhcpnak) with
  | `Noop ->
     Alcotest.fail "client should have wanted to send packets after receiving DHCPNAK in renewal"
  | `New_lease _ ->
     Alcotest.fail "client took a DHCPNAK to mean it had a new lease"
  | `Response (s, buf) ->
     Alcotest.(check (option pass)) "lease is NOT held" (None) (Dhcp_client.lease s);
     parseable buf;
     let dhcpdiscover = Rresult.R.get_ok @@ Dhcp_wire.pkt_of_buf buf (Cstruct.len buf) in
     Alcotest.(check (option msgtype)) "sent DHCPDISCOVER on receiving NAK to renewal request"
       (Some DHCPDISCOVER) (find_message_type dhcpdiscover.options)

let () =
  Alcotest.run "client tests" [
    "state progression", [
       "initializing state machine generates a dhcp packet", `Quick, start_makes_dhcp;
       "dhcp server accepts start packet", `Quick, server_accepts_start_packet;
       "dhcp client doesn't accept DHCPOFFER with wrong xid", `Quick, client_rejects_wrong_xid;
       "dhcp server offers a lease in response to start packet", `Quick, server_gives_dhcpoffer;
       "dhcp client sends a dhcp packet in response to DHCPOFFER", `Quick, client_asks_dhcprequest;
       "dhcp server sends a DHCPACK in response to client DHCPREQUEST", `Quick, server_gives_dhcpack;
       "dhcp client returns lease after receiving DHCPACK", `Quick, client_returns_lease;
       "renewal generates a dhcp packet w/bound client", `Quick, renew_generates_packet;
       "renewal fails on unbound client", `Quick, no_lease_no_renewal;
       "dhcp server accepts renewal request", `Quick, server_accepts_renewal;
       "dhcp client accepts successful lease renewal", `Quick, client_accepts_renewal;
       "dhcp client starts new lease transaction if renewal is declined", `Quick, client_rebinds;
      ]
    ]
