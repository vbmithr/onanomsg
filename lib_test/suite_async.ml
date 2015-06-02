open Core.Std
open Async.Std
open OUnit
open Nanomsg
open Nanomsg_async

let ipc_addr = "/tmp/onanomsg_async_ipc"

let ths = ref []

module E = CCError

let or_fail = function
  | `Ok r -> r
  | `Error (code, msg) -> failwith @@ Printf.sprintf "%s: %s" code msg

let async_socket () =
  let pub = or_fail @@ Nanomsg.socket Pub in
  ignore @@ or_fail @@ Nanomsg.bind pub @@ `Ipc ipc_addr;
  ignore @@ or_fail @@ of_socket_send pub

let pair_test () =
  let msgs = ["auie"; "uie,"; "yx.k"] in
  let addr = `Inproc "rdv_point" in
  let peer1 = CCError.get_exn @@ socket Pair in
  let peer2 = CCError.get_exn @@ socket Pair in
  let _ = CCError.get_exn @@ bind peer1 addr in
  let _ = CCError.get_exn @@ connect peer2 addr in
  Deferred.all_unit @@
  List.map ~f:(fun msg ->
      send_string peer1 msg >>= fun maybe_error_send ->
      recv_string peer2 >>| fun maybe_error_recv ->
      assert_equal msg @@ CCError.get_exn maybe_error_recv
    ) msgs
  >>= fun () ->
  Deferred.all_unit @@
  List.map ~f:(fun msg ->
      send_string peer2 msg >>= fun maybe_error_send ->
      recv_string peer1 >>| fun maybe_error_recv ->
      assert_equal msg @@ CCError.get_exn maybe_error_recv
    ) msgs
  >>| fun () ->
  CCError.get_exn @@ close peer1;
  CCError.get_exn @@ close peer2

let ipc_pubsub_test () =
  let pub = E.get_exn @@ Nanomsg.socket Pub in
  let sub = E.get_exn @@ Nanomsg.socket Sub in
  let pub = E.get_exn @@ of_socket_send pub in
  let sub = E.get_exn @@ of_socket_recv sub in
  let _ = bind pub @@ `Ipc ipc_addr in
  let _ = connect sub @@ `Ipc ipc_addr in
  E.get_exn @@ subscribe (nn_socket sub) "";

  let msg = "bleh" in
  let len = String.length msg in
  let recv_msg = Bytes.create @@ String.length msg in
  let recv_msg' = Bytes.create @@ String.length msg in

  (* NB.send_string *)
  let th = send_string pub msg in
  recv_string sub >>= fun str ->
  assert_equal true (Deferred.is_determined th);
  assert_equal msg @@ E.get_exn str;

  (* NB.send_string_buf *)
  let th = send_string_buf pub msg 0 len in
  recv_bytes_buf sub recv_msg 0 >>= fun nb_recv ->
  assert_equal len @@ E.get_exn nb_recv;
  assert_equal true (Deferred.is_determined th);
  assert_equal msg (Bytes.unsafe_to_string recv_msg);

  (* NB.send_bytes *)
  let th = send_bytes pub recv_msg in
  recv_bytes_buf sub recv_msg' 0 >>| fun nb_recv ->
  assert_equal len @@ E.get_exn nb_recv;
  assert_equal true (Deferred.is_determined th);
  assert_equal recv_msg recv_msg';

  E.get_exn @@ close sub;
  E.get_exn @@ close pub

let run_async_test test_f () =
  let th = test_f () in
  ths := th :: !ths

let test_suite =
  [
    "async_socket", `Quick, async_socket;
    "pair", `Quick, run_async_test pair_test;
    "ipc_pubsub", `Quick, run_async_test ipc_pubsub_test;
  ]

let main () =
  Alcotest.run "ONanomsg async test suite" [
    "test_suite", test_suite;
  ];
  Deferred.all_unit !ths >>= fun () ->
  Sys.remove ipc_addr

let _ =
  Shutdown.don't_finish_before @@ main ();
  never_returns @@ Scheduler.go ()
