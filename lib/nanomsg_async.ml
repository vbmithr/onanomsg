open Core.Std
open Async.Std

open Nanomsg_utils
open Nanomsg

type 'a async_socket = {
  sock: socket;
  sfd: Fd.t;
  rfd: Fd.t;
} [@@deriving create]

let of_socket_recv sock =
  let open CCError in
  recv_fd sock >>= fun rfd ->
  let rfd =
    Fd.create ~avoid_nonblock_if_possible:true
      (Fd.Kind.Socket `Passive) rfd
      Info.(of_string "nanomsg recv_fd") in
  return @@ create_async_socket ~sock ~rfd ~sfd:(Fd.stderr ()) ()

let of_socket_send sock =
  let open CCError in
  send_fd sock >>= fun sfd ->
  let sfd =
    Fd.create ~avoid_nonblock_if_possible:true
      (Fd.Kind.Socket `Passive) sfd
      Info.(of_string "nanomsg send_fd") in
  return @@ create_async_socket ~sock ~rfd:(Fd.stdin ()) ~sfd ()

let of_socket sock =
  let open CCError in
  recv_fd sock >>= fun rfd ->
  send_fd sock >>= fun sfd ->
  let rfd =
    Fd.create ~avoid_nonblock_if_possible:true
      (Fd.Kind.Socket `Passive) rfd
      Info.(of_string "nanomsg recv_fd")
  in
  let sfd =
    Fd.create ~avoid_nonblock_if_possible:true
      (Fd.Kind.Socket `Passive) sfd
      Info.(of_string "nanomsg send_fd")
  in
  return @@ create_async_socket ~sock ~rfd ~sfd ()

let send_buf blitf lenf {sock; sfd; rfd} buf pos len =
  if pos < 0 || len < 0 || pos + len > lenf buf
  then return @@ CCError.fail ("Internal", "bounds")
  else
    C.nn_allocmsg (Unsigned.Size_t.of_int len) 0 |> function
    | None -> return @@ error ()
    | Some nn_buf ->
      let nn_buf_p = Ctypes.(allocate (ptr void) nn_buf) in
      let ba = Ctypes.(bigarray_of_ptr array1 len
                         Bigarray.char @@ from_voidp char nn_buf) in
      blitf ~src:buf ~src_pos:pos ~dst:ba ~dst_pos:0 ~len:len;
      Fd.ready_to sfd `Write >>| function
      | `Bad_fd | `Closed -> CCError.fail ("Internal", "`Bad_fd | `Closed")
      | `Ready ->
        let _ =
          C.nn_send (Obj.magic sock : int)
            nn_buf_p (Unsigned.Size_t.of_int (-1))
            Symbol.(value_of_name_exn "NN_DONTWAIT") in
        CCError.return ()

let send_bigstring_buf = send_buf Bigstring.blit Bigstring.length
let send_bytes_buf = send_buf Bigstring.From_string.blit Bytes.length

let send_bigstring sock buf =
  send_bigstring_buf sock buf 0 @@ CCBigstring.size buf

let send_bytes sock b =
  send_bytes_buf sock b 0 (Bytes.length b)

let send_string_buf sock s pos len =
  send_bytes_buf sock (Bytes.unsafe_of_string s) pos len

let send_string sock s =
  send_bytes_buf sock (Bytes.unsafe_of_string s) 0 (String.length s)

let recv {sock; sfd; rfd} f =
  let open Ctypes in
  let ba_start_p = allocate (ptr void) null in
  Fd.ready_to rfd `Read >>= function
  | `Bad_fd | `Closed ->
    return @@ CCError.fail ("Internal", "`Bad_fd | `Closed")
  | `Ready ->
    let nb_recv = C.nn_recv (Obj.magic sock : int)
        ba_start_p (Unsigned.Size_t.of_int (-1))
        Symbol.(value_of_name_exn "NN_DONTWAIT") in
    let ba_start = !@ ba_start_p in
    let ba = bigarray_of_ptr array1 nb_recv
        Bigarray.char (from_voidp char ba_start) in
    f ba >>| fun res ->
    let (_:int) = C.nn_freemsg ba_start in
    CCError.return res

let recv_bytes_buf sock buf pos =
  recv sock (fun ba ->
      let len = CCBigstring.size ba in
      CCBigstring.blit_to_bytes ba 0 buf pos len;
      return len
    )

let recv_bytes sock =
  recv sock (fun ba ->
      let len = CCBigstring.size ba in
      let buf = Bytes.create len in
      CCBigstring.blit_to_bytes ba 0 buf 0 len;
      return buf
    )

let recv_string sock =
  recv_bytes sock >>| CCError.map Bytes.unsafe_to_string

