module Make(Time : Mirage_time_lwt.S) (Net : Mirage_net_lwt.S) : sig
  type lease = Dhcp_wire.pkt

  type t = lease Lwt_stream.t
  val connect : ?requests:Dhcp_wire.option_code list -> Net.t -> t Lwt.t
end
