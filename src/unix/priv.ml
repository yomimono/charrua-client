let set_config lease =
  (* put your actual lease-handling code (comms with privsep container, etc) here *)
  Printf.printf "Lease acquired!! %s\n%!" @@ Dhcp_wire.pkt_to_string lease;
  Lwt.return_unit
