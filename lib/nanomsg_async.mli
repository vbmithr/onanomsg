open Core.Std
open Async.Std
open Nanomsg

type 'a async_socket
val of_socket_send : socket -> ([`Send] async_socket, error) CCError.t
val of_socket_recv : socket -> ([`Recv] async_socket, error) CCError.t
val of_socket : socket -> ([`Send | `Recv] async_socket, error) CCError.t

(** {1 Asynchronous I/O} *)

(** {2 Zero-copy I/O} *)

val send_bigstring : [`Send] async_socket -> Bigstring.t -> (unit, error) CCError.t Deferred.t

val send_bigstring_buf : [`Send] async_socket -> Bigstring.t -> int -> int ->
  (unit, error) CCError.t  Deferred.t

val send_string : [`Send] async_socket -> string -> (unit, error) CCError.t  Deferred.t

val send_string_buf : [`Send] async_socket -> string -> int -> int ->
  (unit, error) CCError.t Deferred.t

val send_bytes : [`Send] async_socket -> Bytes.t -> (unit, error) CCError.t Deferred.t

val send_bytes_buf : [`Send] async_socket -> Bytes.t -> int -> int ->
  (unit, error) CCError.t Deferred.t

val recv : [`Recv] async_socket -> (Bigstring.t -> 'a Deferred.t) -> ('a, error) CCError.t Deferred.t
(** [recv sock f] applies [f] to the received message. The
    argument of [f] gets unallocated after [f] returns, so make sure
    [f] {b never} let a reference to its argument escape. *)

(** {2 Legacy I/O} *)

val recv_string : [`Recv] async_socket -> (string, error) CCError.t Deferred.t
val recv_bytes : [`Recv] async_socket -> (Bytes.t, error) CCError.t  Deferred.t
val recv_bytes_buf : [`Recv] async_socket -> Bytes.t -> int -> (int, error) CCError.t Deferred.t
