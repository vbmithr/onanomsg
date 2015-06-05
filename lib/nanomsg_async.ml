open Core.Std
open Async.Std

open Nanomsg_utils
open Nanomsg

type +'a socket = {
  sock: 'a Nanomsg.socket;
  sfd: Fd.t;
  rfd: Fd.t;
} constraint 'a = [< `Send | `Recv] [@@deriving create]

type 'a or_error = [ `Ok of 'a | `Error of error ]

let fd_create_memo_passive =
  Memo.general ~hashable:Core.Std.Unix.File_descr.hashable
    (fun fd ->
       Fd.create ~avoid_nonblock_if_possible:true
         (Fd.Kind.Socket `Passive) fd
         Info.(of_string "nanomsg recv_fd")
    )

let fd_create_memo_active =
  Memo.general ~hashable:Core.Std.Unix.File_descr.hashable
    (fun fd ->
       Fd.create ~avoid_nonblock_if_possible:true
         (Fd.Kind.Socket `Active) fd
         Info.(of_string "nanomsg send_fd")
    )

let of_socket_ro sock =
  let open CCError in
  recv_fd sock >|= fun rfd ->
  let rfd = fd_create_memo_passive rfd in
  create_socket ~sock ~rfd ~sfd:rfd ()

let of_socket_wo sock =
  let open CCError in
  send_fd sock >|= fun sfd ->
  let sfd = fd_create_memo_active sfd in
  create_socket ~sock ~rfd:sfd ~sfd ()

let of_socket_rw sock =
  let open CCError in
  recv_fd sock >>= fun rfd ->
  send_fd sock >|= fun sfd ->
  let rfd, sfd =
    let rfd_ = fd_create_memo_passive rfd in
    if rfd = sfd then
      (rfd_, rfd_)
    else
      let sfd_ = fd_create_memo_active sfd in
      (rfd_, sfd_)
  in
  create_socket ~sock ~rfd ~sfd ()

let socket_ro ?domain proto = CCError.(socket_ro ?domain proto >>= of_socket_ro)
let socket_wo ?domain proto = CCError.(socket_wo ?domain proto >>= of_socket_wo)
let socket_rw ?domain proto = CCError.(socket_rw ?domain proto >>= of_socket_rw)

let bind {sock; _} addr = bind sock addr
let connect {sock; _} addr = connect sock addr
let shutdown {sock; _} eid = shutdown sock eid
let close {sock; _} = close sock

let with_nn_sock {sock; _} f = f sock

let send blitf lenf {sock; sfd; rfd} buf pos len =
  if pos < 0 || len < 0 || pos + len > lenf buf
  then return @@ `Error ("OCaml", "bounds")
  else
    C.nn_allocmsg (Unsigned.Size_t.of_int len) 0 |> function
    | None -> return @@ error ()
    | Some nn_buf ->
      let nn_buf_p = Ctypes.(allocate (ptr void) nn_buf) in
      let ba = Ctypes.(bigarray_of_ptr array1 len
                         Bigarray.char @@ from_voidp char nn_buf) in
      blitf ~src:buf ~src_pos:pos ~dst:ba ~dst_pos:0 ~len:len;
      Fd.ready_to sfd `Write >>| function
      | `Bad_fd | `Closed -> `Error ("OCaml", "`Bad_fd | `Closed")
      | `Ready ->
        error_if_negative
          (fun () -> C.nn_send sock
              nn_buf_p (Unsigned.Size_t.of_int (-1))
              Symbol.(value_of_name_exn "NN_DONTWAIT")
          )

let send_bigstring_buf = send Bigstring.blit Bigstring.length
let send_bytes_buf = send Bigstring.From_string.blit Bytes.length

let send_bigstring sock buf =
  send_bigstring_buf sock buf 0 @@ CCBigstring.length buf

let send_bytes sock b =
  send_bytes_buf sock b 0 (Bytes.length b)

let send_string_buf sock s pos len =
  send_bytes_buf sock (Bytes.unsafe_of_string s) pos len

let send_string sock s =
  send_bytes_buf sock (Bytes.unsafe_of_string s) 0 (String.length s)

let recv {sock; sfd; rfd} f =
  let open Ctypes in
  let ba_start_p = allocate (ptr void) null in
  Fd.ready_to rfd `Read >>| function
  | `Bad_fd | `Closed -> `Error ("OCaml", "`Bad_fd | `Closed")
  | `Ready ->
    error_if_negative
      (fun () ->
         C.nn_recv sock
           ba_start_p (Unsigned.Size_t.of_int (-1))
           Symbol.(value_of_name_exn "NN_DONTWAIT")
      )
    |> function
    | `Error (a, b) -> `Error (a, b)
    | `Ok nb_recv ->
      let ba_start = !@ ba_start_p in
      let ba = bigarray_of_ptr array1 nb_recv
          Bigarray.char (from_voidp char ba_start) in
      let res = f ba in
      CCError.(
        error_if_negative (fun () -> C.nn_freemsg ba_start)
        >|= fun _ -> res)

let recv_bytes_buf sock buf pos =
  recv sock (fun ba ->
      let len = CCBigstring.length ba in
      CCBigstring.blit_to_bytes ba 0 buf pos len;
      len
    )

let recv_bytes sock =
  recv sock (fun ba ->
      let len = CCBigstring.length ba in
      let buf = Bytes.create len in
      CCBigstring.blit_to_bytes ba 0 buf 0 len;
      buf
    )

let recv_string sock =
  recv_bytes sock >>| function
  | `Error exn -> `Error exn
  | `Ok b -> `Ok (Bytes.unsafe_to_string b)
