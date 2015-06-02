open Lwt.Infix

open Nanomsg_utils
open Nanomsg

exception Error of string * string

type +'a socket = {
  sock: Nanomsg.socket;
  sfd: Lwt_unix.file_descr;
  rfd: Lwt_unix.file_descr;
} constraint 'a = [< `Send | `Recv] [@@deriving create]

let wrap_error = function
  | `Error (name, descr) -> Lwt.fail @@ Error (name, descr)
  | `Ok a -> Lwt.return a

let bind_error f = function
  | `Error (name, descr) -> Lwt.fail @@ Error (name, descr)
  | `Ok a -> f a

let map_error f = function
  | `Error (name, descr) -> Lwt.fail @@ Error (name, descr)
  | `Ok a -> Lwt.return (f a)

let fail () =
  let `Error (a, b) = error () in
  Lwt.fail @@ Error (a, b)

let of_socket_recv sock =
  wrap_error (recv_fd sock) >>= fun rfd ->
  let rfd =
    Lwt_unix.of_unix_file_descr ~blocking:false rfd in
  Lwt.return @@ create_socket ~sock ~rfd ~sfd:(Lwt_unix.stderr) ()

let of_socket_send sock =
  wrap_error (send_fd sock) >>= fun sfd ->
    let sfd =
      Lwt_unix.of_unix_file_descr ~blocking:false sfd in
    Lwt.return @@ create_socket ~sock ~rfd:(Lwt_unix.stdin) ~sfd ()

let of_socket sock =
  wrap_error (recv_fd sock) >>= fun rfd ->
  wrap_error (send_fd sock) >>= fun sfd ->
  let rfd = Lwt_unix.of_unix_file_descr ~blocking:false rfd in
  let sfd = Lwt_unix.of_unix_file_descr ~blocking:false sfd in
  Lwt.return @@ create_socket ~sock ~rfd ~sfd ()

let socket ?domain proto =
  wrap_error @@ socket ?domain proto >>= of_socket

let bind sock addr = wrap_error @@ bind sock.sock addr
let connect sock addr = wrap_error @@ connect sock.sock addr
let shutdown sock eid = wrap_error @@ shutdown sock.sock eid
let close sock = wrap_error @@ close sock.sock
let nn_socket sock = sock.sock

let wait_read sock f = Lwt_unix.(wrap_syscall Read sock.rfd f)
let wait_write sock f = Lwt_unix.(wrap_syscall Write sock.sfd f)

let send_buf blitf lenf sock buf pos len =
  if pos < 0 || len < 0 || pos + len > lenf buf
  then Lwt.fail @@ Error ("Internal", "bounds")
  else
    let nn_buf = C.nn_allocmsg (Unsigned.Size_t.of_int len) 0 in
    match nn_buf with
    | None -> fail ()
    | Some nn_buf ->
      let nn_buf_p = Ctypes.(allocate (ptr void) nn_buf) in
      let ba = Ctypes.(bigarray_of_ptr array1 len
                         Bigarray.char @@ from_voidp char nn_buf) in
      blitf buf pos ba 0 len;
      wait_write sock
        (fun () -> C.nn_send (Obj.magic sock.sock : int)
            nn_buf_p (Unsigned.Size_t.of_int (-1))
            Symbol.(value_of_name_exn "NN_DONTWAIT")) >>= fun nb_written ->
      if nb_written <> len then fail ()
      else Lwt.return_unit

let send_bigstring_buf = send_buf CCBigstring.blit CCBigstring.size
let send_bytes_buf = send_buf CCBigstring.blit_of_bytes Bytes.length

let send_bigstring sock buf =
  send_bigstring_buf sock buf 0 @@ CCBigstring.size buf

let send_bytes sock b =
  send_bytes_buf sock b 0 (Bytes.length b)

let send_string_buf sock s pos len =
  send_bytes_buf sock (Bytes.unsafe_of_string s) pos len

let send_string sock s =
  send_bytes_buf sock (Bytes.unsafe_of_string s) 0 (String.length s)

let recv sock f =
  let open Ctypes in
  let ba_start_p = allocate (ptr void) null in
  wait_read sock
    (fun () -> C.nn_recv (Obj.magic sock.sock : int)
        ba_start_p (Unsigned.Size_t.of_int (-1))
        Symbol.(value_of_name_exn "NN_DONTWAIT")) >>= fun nb_recv ->
  let ba_start = !@ ba_start_p in
  let ba = bigarray_of_ptr array1 nb_recv
      Bigarray.char (from_voidp char ba_start) in
  f ba >|= fun res ->
  let (_:int) = C.nn_freemsg ba_start in
  res

let recv_bytes_buf sock buf pos =
  recv sock (fun ba ->
      let len = CCBigstring.size ba in
      CCBigstring.blit_to_bytes ba 0 buf pos len;
      Lwt.return len
    )

let recv_bytes sock =
  recv sock (fun ba ->
      let len = CCBigstring.size ba in
      let buf = Bytes.create len in
      CCBigstring.blit_to_bytes ba 0 buf 0 len;
      Lwt.return buf
    )

let recv_string sock = recv_bytes sock >|= Bytes.unsafe_to_string

