type domain = AF_SP | AF_SP_RAW

type _ proto =
  | Pair : [`Send | `Recv] proto
  | Pub : [`Send] proto
  | Sub : [`Recv] proto
  | Req : [`Send | `Recv] proto
  | Rep : [`Send | `Recv] proto
  | Push : [`Send] proto
  | Pull : [`Recv] proto
  | Surveyor : [`Send | `Recv] proto
  | Respondant : [`Send | `Recv] proto
  | Bus : [`Send | `Recv] proto

val proto_ro_to_enum : [`Recv] proto -> int
val proto_wo_to_enum : [`Send] proto -> int
val proto_rw_to_enum : [`Send | `Recv] proto -> int

type +'a socket = int constraint 'a = [< `Send | `Recv]

module Addr : sig
  type bind = [
    | `All
    | `V4 of Ipaddr.V4.t
    | `V6 of Ipaddr.V6.t
    | `Iface of string ]
      [@@deriving show]

  type connect =
    [`V4 of Ipaddr.V4.t | `V6 of Ipaddr.V6.t | `Dns of string] *
    [`V4 of Ipaddr.V4.t | `V6 of Ipaddr.V6.t | `Iface of string] option
      [@@deriving show]

  type 'a t = [
    | `Inproc of string
    | `Ipc of string
    | `Tcp of 'a * int
  ] [@@deriving show]

  val bind_of_string : string -> bind t
  val bind_to_string : bind t -> string
  val connect_of_string : string -> connect t
  val connect_to_string : connect t -> string
end

type eid

(** {1 Exceptions} *)

(** {1 Socket management } *)
type error = string * string
val socket_ro : ?domain:domain -> [`Recv] proto -> ([`Recv] socket, error) CCError.t
val socket_wo : ?domain:domain -> [`Send] proto -> ([`Send] socket, error) CCError.t
val socket_rw : ?domain:domain -> [`Recv | `Send] proto -> ([`Recv | `Send] socket, error) CCError.t

val bind : [< `Send | `Recv] socket -> Addr.bind Addr.t -> (eid, error) CCError.t
val connect : [< `Send | `Recv] socket -> Addr.connect Addr.t -> (eid, error) CCError.t
val shutdown : [< `Send | `Recv] socket -> eid -> (unit, error) CCError.t
val close : [< `Send | `Recv] socket -> (unit, error) CCError.t

(** {1 I/O } *)

(** {2 Zero-copy I/O} *)

val send_bigstring : ?block:bool -> [> `Send] socket -> CCBigstring.t -> (unit, error) CCError.t
val send_bigstring_buf : ?block:bool -> [> `Send] socket -> CCBigstring.t -> int -> int -> (unit, error) CCError.t
val send_string : ?block:bool -> [> `Send] socket -> string -> (unit, error) CCError.t
val send_string_buf : ?block:bool -> [> `Send] socket -> string -> int -> int -> (unit, error) CCError.t
val send_bytes : ?block:bool -> [> `Send] socket -> Bytes.t -> (unit, error) CCError.t
val send_bytes_buf : ?block:bool -> [> `Send] socket -> Bytes.t -> int -> int -> (unit, error) CCError.t

val recv : ?block:bool -> [> `Recv] socket -> (CCBigstring.t -> 'a) -> ('a, error) CCError.t
(** [recv ?block sock f] applies [f] to the received message. The
    argument of [f] gets unallocated after [f] returns, so make sure
    [f] {b never} let a reference to its argument escape. *)

(** {2 Legacy I/O} *)

val recv_string : ?block:bool -> [> `Recv] socket -> (string, error) CCError.t
val recv_bytes : ?block:bool -> [> `Recv] socket -> (Bytes.t, error) CCError.t
val recv_bytes_buf :?block:bool -> [> `Recv] socket -> Bytes.t -> int -> (int, error) CCError.t

(** {1 Get socket options} *)

val domain : [< `Send | `Recv] socket -> (domain, error) CCError.t
val proto_int : [< `Send | `Recv] socket -> (int, error) CCError.t
val send_fd : [< `Send | `Recv] socket -> (Unix.file_descr, error) CCError.t
val recv_fd : [< `Send | `Recv] socket -> (Unix.file_descr, error) CCError.t

val get_linger : [< `Send | `Recv] socket -> ([`Inf | `Ms of int], error) CCError.t
val get_send_bufsize : [< `Send | `Recv] socket -> (int, error) CCError.t
val get_recv_bufsize : [< `Send | `Recv] socket -> (int, error) CCError.t
val get_send_timeout : [< `Send | `Recv] socket -> ([`Inf | `Ms of int], error) CCError.t
val get_recv_timeout : [< `Send | `Recv] socket -> ([`Inf | `Ms of int], error) CCError.t
val get_reconnect_ival : [< `Send | `Recv] socket -> (int, error) CCError.t
val get_reconnect_ival_max : [< `Send | `Recv] socket -> (int, error) CCError.t
val get_send_prio : [< `Send | `Recv] socket -> (int, error) CCError.t
val get_recv_prio : [< `Send | `Recv] socket -> (int, error) CCError.t
val get_ipv4only : [< `Send | `Recv] socket -> (bool, error) CCError.t

(** {1 Set socket options} *)

(** {2 General} *)

val set_linger : [< `Send | `Recv] socket -> [`Inf | `Ms of int] -> (unit, error) CCError.t
val set_send_bufsize : [< `Send | `Recv] socket -> int -> (unit, error) CCError.t
val set_recv_bufsize : [< `Send | `Recv] socket -> int -> (unit, error) CCError.t
val set_send_timeout : [< `Send | `Recv] socket -> [`Inf | `Ms of int] -> (unit, error) CCError.t
val set_recv_timeout : [< `Send | `Recv] socket -> [`Inf | `Ms of int] -> (unit, error) CCError.t
val set_reconnect_ival : [< `Send | `Recv] socket -> int -> (unit, error) CCError.t
val set_reconnect_ival_max : [< `Send | `Recv] socket -> int -> (unit, error) CCError.t
val set_send_prio : [< `Send | `Recv] socket -> int -> (unit, error) CCError.t
val set_recv_prio : [< `Send | `Recv] socket -> int -> (unit, error) CCError.t
val set_ipv4_only : [< `Send | `Recv] socket -> bool -> (unit, error) CCError.t

(** {2 PubSub} *)

val subscribe : [> `Recv] socket -> string -> (unit, error) CCError.t
val unsubscribe : [> `Recv] socket -> string -> (unit, error) CCError.t

(** {1 Termination} *)

val term : unit -> unit
