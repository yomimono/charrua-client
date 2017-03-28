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


let get_lease ?(mac=random_mac ()) ic oc =
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

(* renew_lease will try ad inifinitum to renew an existing lease;
 * it needs external controls to recognize that it has entered the REBINDING
 * period and should give up and begin a new lease transaction *)
let renew_lease ic oc client =
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

(* a pretty bad text adventure *)
let () =
  set_binary_mode_in stdin true;
  set_binary_mode_out stdout true;
  try
    let (client, lease) = get_lease stdin stdout in
    Printf.eprintf "You win!: %s\n%!" @@ Dhcp_wire.pkt_to_string lease;
    Printf.eprintf "%s\n%!" "Get ready for bonus round!!!";
    match renew_lease stdin stdout client with
    | Some (_c, new_lease) ->
      Printf.eprintf "%s\n%!" "BONUS ROUND WIN!!!\n%!";
      Printf.eprintf "%s\n%!" @@ Dhcp_wire.pkt_to_string new_lease
    | None ->
      Printf.eprintf "%s\n%!" "you lost the bonus round :("
  with
  | End_of_file -> exit 1
