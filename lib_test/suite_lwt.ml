open OUnit2
open Lwt.Infix
open Nanomsg
open Nanomsg_lwt

let pair_test () =
  let msgs = ["auie"; "uie,"; "yx.k"] in
  let addr = `Inproc "rdv_point" in
  socket_rw Pair >>= fun peer1 ->
  socket_rw Pair >>= fun peer2 ->
  bind peer1 addr >>= fun _ ->
  connect peer2 addr >>= fun _ ->
  Lwt_list.iter_s (fun msg ->
      send_string peer1 msg >>= fun () ->
      recv_string peer2 >|= fun recv_msg ->
      assert_equal msg recv_msg
    ) msgs >>= fun () ->
  Lwt_list.iter_s (fun msg ->
      send_string peer2 msg >>= fun () ->
      recv_string peer1 >|= fun recv_msg ->
      assert_equal msg recv_msg
    ) msgs >>= fun () ->
  close peer1 >>= fun () ->
  close peer2

let tcp_pubsub_test () =
  let port = 56352 in
  socket_wo Pub >>= fun pub ->
  socket_ro Sub >>= fun sub ->
  let _ = set_ipv4_only (nn_socket pub) false in
  let _ = set_ipv4_only (nn_socket sub) false in
  let _ = bind pub @@ `Tcp (`All, port) in
  let _ = connect sub @@ `Tcp ((`V6 Ipaddr.V6.localhost, None), port) in
  let _ = subscribe (nn_socket sub) "" in
  let msg = "bleh" in
  let len = String.length msg in
  let recv_msg = Bytes.create @@ String.length msg in
  let recv_msg' = Bytes.create @@ String.length msg in
  let open Lwt.Infix in

  (* send_string *)
  let th = send_string pub msg in
  recv_string sub >>= fun str ->
  assert_equal (Lwt.Return ()) (Lwt.state th);
  assert_equal msg str;

  (* send_string_buf *)
  let th = send_string_buf pub msg 0 len in
  recv_bytes_buf sub recv_msg 0 >>= fun nb_recv ->
  assert_equal nb_recv len;
  assert_equal (Lwt.Return ()) (Lwt.state th);
  assert_equal msg (Bytes.unsafe_to_string recv_msg);

  (* send_bytes *)
  let th = send_bytes pub recv_msg in
  recv_bytes_buf sub recv_msg' 0 >>= fun nb_recv ->
  assert_equal nb_recv len;
  assert_equal (Lwt.Return ()) (Lwt.state th);
  assert_equal recv_msg recv_msg';

  close pub >>= fun () ->
  close sub

let pipeline_local_test () =
  let msgs = ["foo"; "bar"; "baz"] in
  let receiver addr =
    socket_ro Pull >>= fun s ->
    bind s addr >>= fun _ ->
    Lwt_list.iter_s
      (fun msg ->
         recv_string s >|= fun m ->
         assert_equal
           ~msg:"pipeline_local_test"
           ~printer:(fun i -> i) msg m
      ) msgs
  in
  let sender addr =
    socket_wo Push >>= fun s ->
    connect s addr >>= fun _ ->
    Lwt_list.iter_s (send_string s) msgs >>= fun () ->
    Lwt_unix.yield () >>= fun () -> close s
  in
  Lwt.join
    [
      sender (`Inproc "rdvpoint");
      receiver (`Inproc "rdvpoint")
    ]

let tests = [ pair_test; (* tcp_pubsub_test; *) pipeline_local_test ]

let _ = Lwt_main.run @@ Lwt_list.iter_s (fun t -> t ()) tests
