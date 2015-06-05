open Core.Std
open Async.Std
open Nanomsg

type +'a socket constraint 'a = [< `Send | `Recv]

type 'a or_error = [ `Ok of 'a | `Error of error]

val of_socket_ro : [`Recv] Nanomsg.socket -> ([`Recv] socket, error) CCError.t
val of_socket_wo : [`Send] Nanomsg.socket -> ([`Send] socket, error) CCError.t
val of_socket_rw : 'a Nanomsg.socket -> ('a socket, error) CCError.t

val socket_ro : ?domain:domain -> [`Recv] proto -> ([`Recv] socket, error) CCError.t
val socket_wo : ?domain:domain -> [`Send] proto -> ([`Send] socket, error) CCError.t
val socket_rw : ?domain:domain -> [`Recv | `Send] proto -> ([`Recv | `Send] socket, error) CCError.t

val bind : 'a socket -> Addr.bind Addr.t -> (eid, error) CCError.t
val connect : 'a socket -> Addr.connect Addr.t -> (eid, error) CCError.t
val shutdown : 'a socket -> eid -> (unit, error) CCError.t
val close : 'a socket -> (unit, error) CCError.t

val with_nn_sock : 'a socket -> ('a Nanomsg.socket -> 'b) -> 'b

(** {1 Asynchronous I/O} *)

(** {2 Zero-copy I/O} *)

(** All the send_ functions return the number of bytes actually sent. *)

val send_bigstring : [> `Send] socket -> Bigstring.t -> int or_error Deferred.t
val send_bigstring_buf : [> `Send] socket -> Bigstring.t -> int -> int -> int or_error Deferred.t
val send_string : [> `Send] socket -> string -> int or_error Deferred.t
val send_string_buf : [> `Send] socket -> string -> int -> int -> int or_error Deferred.t
val send_bytes : [> `Send] socket -> Bytes.t -> int or_error Deferred.t
val send_bytes_buf : [> `Send] socket -> Bytes.t -> int -> int -> int or_error Deferred.t

val recv : [> `Recv] socket -> (Bigstring.t -> 'a) -> 'a or_error Deferred.t

(** [recv sock f] applies [f] to the received message. The
    argument of [f] gets unallocated after [f] returns, so make sure
    [f] {b never} let a reference to its argument escape. *)

(** {2 Legacy I/O} *)

val recv_string : [> `Recv] socket -> string or_error Deferred.t
val recv_bytes : [> `Recv] socket -> Bytes.t or_error Deferred.t
val recv_bytes_buf : [> `Recv] socket -> Bytes.t -> int -> int or_error Deferred.t
