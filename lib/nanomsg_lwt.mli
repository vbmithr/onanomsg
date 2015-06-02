open Nanomsg

val wrap_error : ('a, error) CCError.t -> 'a Lwt.t
val bind_error : ('a -> 'b Lwt.t) -> ('a, error) CCError.t -> 'b Lwt.t
val map_error : ('a -> 'b) -> ('a, error) CCError.t -> 'b Lwt.t

type +'a socket constraint 'a = [< `Send | `Recv]

val socket : ?domain:domain -> ([< `Send | `Recv] as 'a) proto -> 'a socket Lwt.t
val bind : [< `Send | `Recv] socket -> Addr.bind Addr.t -> eid Lwt.t
val connect : [< `Send | `Recv] socket -> Addr.connect Addr.t -> eid Lwt.t
val shutdown : [< `Send | `Recv] socket -> eid -> unit Lwt.t
val close : [< `Send | `Recv] socket -> unit Lwt.t

val nn_socket : ([< `Send | `Recv] as 'a) socket -> 'a Nanomsg.socket
val of_socket : ([< `Send | `Recv] as 'a) Nanomsg.socket -> 'a socket Lwt.t

(** {1 Asynchronous I/O} *)

(** {2 Zero-copy I/O} *)

val send_bigstring : [> `Send] socket -> CCBigstring.t -> unit Lwt.t
val send_bigstring_buf : [> `Send] socket -> CCBigstring.t -> int -> int -> unit Lwt.t

val send_string : [> `Send] socket -> string -> unit Lwt.t
val send_string_buf : [> `Send] socket -> string -> int -> int -> unit Lwt.t

val send_bytes : [> `Send] socket -> Bytes.t -> unit Lwt.t
val send_bytes_buf : [> `Send] socket -> Bytes.t -> int -> int -> unit Lwt.t

val recv : [> `Recv] socket -> (CCBigstring.t -> 'a Lwt.t) -> 'a Lwt.t
(** [recv sock f] applies [f] to the received message. The
    argument of [f] gets unallocated after [f] returns, so make sure
    [f] {b never} let a reference to its argument escape. *)

(** {2 Legacy I/O} *)

val recv_string : [> `Recv] socket -> string Lwt.t
val recv_bytes : [> `Recv] socket -> Bytes.t Lwt.t
val recv_bytes_buf : [> `Recv] socket -> Bytes.t -> int -> int Lwt.t
