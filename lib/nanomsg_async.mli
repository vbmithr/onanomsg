open Core.Std
open Async.Std
open Nanomsg

type +'a socket constraint 'a = [< `Send | `Recv]

val of_socket_ro : [`Recv] Nanomsg.socket -> ([`Recv] socket, error) CCError.t
val of_socket_wo : [`Send] Nanomsg.socket -> ([`Send] socket, error) CCError.t
val of_socket_rw : [`Send | `Recv] Nanomsg.socket -> ([`Send | `Recv] socket, error) CCError.t

val socket_ro : ?domain:domain -> [`Recv] proto -> ([`Recv] socket, error) CCError.t
val socket_wo : ?domain:domain -> [`Send] proto -> ([`Send] socket, error) CCError.t
val socket_rw : ?domain:domain -> [`Recv | `Send] proto -> ([`Recv | `Send] socket, error) CCError.t

val bind : [< `Send | `Recv] socket -> Addr.bind Addr.t -> (eid, error) CCError.t
val connect : [< `Send | `Recv] socket -> Addr.connect Addr.t -> (eid, error) CCError.t
val shutdown : [< `Send | `Recv] socket -> eid -> (unit, error) CCError.t
val close : [< `Send | `Recv] socket -> (unit, error) CCError.t

val nn_socket : ([< `Send | `Recv] as 'a) socket -> 'a Nanomsg.socket

(** {1 Asynchronous I/O} *)

(** {2 Zero-copy I/O} *)

val send_bigstring : [> `Send] socket -> Bigstring.t -> (unit, error) CCError.t Deferred.t

val send_bigstring_buf : [> `Send] socket -> Bigstring.t -> int -> int ->
  (unit, error) CCError.t  Deferred.t

val send_string : [> `Send] socket -> string -> (unit, error) CCError.t  Deferred.t

val send_string_buf : [> `Send] socket -> string -> int -> int ->
  (unit, error) CCError.t Deferred.t

val send_bytes : [> `Send] socket -> Bytes.t -> (unit, error) CCError.t Deferred.t

val send_bytes_buf : [> `Send] socket -> Bytes.t -> int -> int ->
  (unit, error) CCError.t Deferred.t

val recv : [> `Recv] socket -> (Bigstring.t -> 'a Deferred.t) -> ('a, error) CCError.t Deferred.t
(** [recv sock f] applies [f] to the received message. The
    argument of [f] gets unallocated after [f] returns, so make sure
    [f] {b never} let a reference to its argument escape. *)

(** {2 Legacy I/O} *)

val recv_string : [> `Recv] socket -> (string, error) CCError.t Deferred.t
val recv_bytes : [> `Recv] socket -> (Bytes.t, error) CCError.t  Deferred.t
val recv_bytes_buf : [> `Recv] socket -> Bytes.t -> int -> (int, error) CCError.t Deferred.t
