exception Error of string * string

type d = [`AF_SP | `AF_SP_RAW]
type p = [`Pair | `Pub | `Sub | `Req | `Rep | `Push | `Pull | `Surveyor | `Respondant | `Bus]
type p_snd = [`Pair | `Pub | `Req | `Rep | `Push | `Surveyor | `Respondant | `Bus]
type p_rcv = [`Pair | `Sub | `Req | `Rep | `Pull | `Surveyor | `Respondant | `Bus]

type addr = [`Inproc of string | `Ipc of string | `Tcp of Ipaddr.t * int]
val string_of_addr : addr -> string
val addr_of_string : string -> addr

type ('d, 'p) socket
type eid

val socket : ([< d] as 'd) -> ([< p] as 'p) -> ('d, 'p) socket

val bind : ([< d], [< p]) socket -> addr -> eid
val connect : ([< d], [< p]) socket -> addr -> eid

module B : sig
  val send : ([< d], [< p_snd]) socket -> Lwt_bytes.t -> int -> int -> unit
  val send_from_bytes : ([< d], [< p_snd]) socket -> Bytes.t -> int -> int -> unit
  val send_from_string : ([< d], [< p_snd]) socket -> string -> unit

  val recv : ([< d], [< p_rcv]) socket -> (Lwt_bytes.t -> int -> 'a) -> 'a
  val recv_to_string : ([< d], [< p_rcv]) socket -> (string -> 'a) -> 'a
end

module NB : sig
  val send : ([< d], [< p_snd]) socket -> Lwt_bytes.t -> int -> int -> unit Lwt.t
  val send_from_bytes : ([< d], [< p_snd]) socket -> Bytes.t -> int -> int -> unit Lwt.t
  val send_from_string : ([< d], [< p_snd]) socket -> string -> unit Lwt.t

  val recv : ([< d], [< p_rcv]) socket -> (Lwt_bytes.t -> int -> 'a Lwt.t) -> 'a Lwt.t
  val recv_to_string : ([< d], [< p_rcv]) socket -> (string -> 'a Lwt.t) -> 'a Lwt.t
end

val shutdown : ([< d], [< p]) socket -> eid -> unit
val close : ([< d], [< p]) socket -> unit

(** {1 Publish-Subscribe} *)

val subscribe : ([< `AF_SP], [< `Sub]) socket -> string -> unit
val unsubscribe : ([< `AF_SP], [< `Sub]) socket -> string -> unit

(** {1 Get socket options} *)

val domain : ([< d], [< p]) socket -> d
val proto : ([< d], [< p]) socket -> p
val send_fd : ([< d], [< p_snd]) socket -> Unix.file_descr
val recv_fd : ([< d], [< p_rcv]) socket -> Unix.file_descr

val get_linger : ([< d], [< p]) socket -> [`Inf | `Ms of int]
val get_send_bufsize : ([< d], [< p]) socket -> int
val get_recv_bufsize : ([< d], [< p]) socket -> int
val get_send_timeout : ([< d], [< p]) socket -> [`Inf | `Ms of int]
val get_recv_timeout : ([< d], [< p]) socket -> [`Inf | `Ms of int]
val get_reconnect_ival : ([< d], [< p]) socket -> int
val get_reconnect_ival_max : ([< d], [< p]) socket -> int
val get_send_prio : ([< d], [< p]) socket -> int
val get_recv_prio : ([< d], [< p]) socket -> int
val get_ipv4only : ([< d], [< p]) socket -> bool

(** {1 Set socket options} *)

val set_linger : ([< d], [< p]) socket -> [`Inf | `Ms of int] -> unit
val set_send_bufsize : ([< d], [< p]) socket -> int -> unit
val set_recv_bufsize : ([< d], [< p]) socket -> int -> unit
val set_send_timeout : ([< d], [< p]) socket -> [`Inf | `Ms of int] -> unit
val set_recv_timeout : ([< d], [< p]) socket -> [`Inf | `Ms of int] -> unit
val set_reconnect_ival : ([< d], [< p]) socket -> int -> unit
val set_reconnect_ival_max : ([< d], [< p]) socket -> int -> unit
val set_send_prio : ([< d], [< p]) socket -> int -> unit
val set_recv_prio : ([< d], [< p]) socket -> int -> unit
val set_ipv4_only : ([< d], [< p]) socket -> bool -> unit

(** {1 Termination} *)

val term : unit -> unit
