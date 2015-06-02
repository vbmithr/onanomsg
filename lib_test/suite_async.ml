open Core.Std
open Async.Std
open Nanomsg
open OUnit
module A = Nanomsg_async

let ipc_addr = "/tmp/onanomsg_async_ipc"

let ths = ref []

module E = CCError

let or_fail = function
  | `Ok r -> r
  | `Error (code, msg) -> failwith @@ Printf.sprintf "%s: %s" code msg

let async_socket () =
  let pub = or_fail @@ socket Pub in
  ignore @@ or_fail @@ bind pub @@ `Ipc ipc_addr;
  ignore @@ or_fail @@ A.of_socket_send pub

let ipc_pubsub_test () =
  let ivar = Ivar.create () in
  ths := ivar :: !ths;
  let pub = E.get_exn @@ socket Pub in
  let sub = E.get_exn @@ socket Sub in
  let apub = E.get_exn @@ A.of_socket_send pub in
  let asub = E.get_exn @@ A.of_socket_recv sub in
  let _ = bind pub @@ `Ipc ipc_addr in
  let _ = connect sub @@ `Ipc ipc_addr in
  E.get_exn @@ subscribe sub "";

  let msg = "bleh" in
  let len = String.length msg in
  let recv_msg = Bytes.create @@ String.length msg in
  let recv_msg' = Bytes.create @@ String.length msg in

  (* NB.send_string *)
  let th = A.send_string apub msg in
  A.recv_string asub >>= fun str ->
  assert_equal true (Deferred.is_determined th);
  assert_equal msg @@ E.get_exn str;

  (* NB.send_string_buf *)
  let th = A.send_string_buf apub msg 0 len in
  A.recv_bytes_buf asub recv_msg 0 >>= fun nb_recv ->
  assert_equal len @@ E.get_exn nb_recv;
  assert_equal true (Deferred.is_determined th);
  assert_equal msg (Bytes.unsafe_to_string recv_msg);

  (* NB.send_bytes *)
  let th = A.send_bytes apub recv_msg in
  A.recv_bytes_buf asub recv_msg' 0 >>| fun nb_recv ->
  assert_equal len @@ E.get_exn nb_recv;
  assert_equal true (Deferred.is_determined th);
  assert_equal recv_msg recv_msg';

  E.get_exn @@ close sub;
  E.get_exn @@ close pub;
  Ivar.fill ivar ()

let test_suite =
  [
    "async_socket", `Quick, async_socket;
    "ipc_pubsub_test", `Quick, fun () -> don't_wait_for @@ ipc_pubsub_test ();
  ]

let main () =
  Alcotest.run "ONanomsg async test suite" [
    "test_suite", test_suite;
  ];
  Deferred.all_unit (List.map !ths ~f:Ivar.read) >>= fun () ->
  Sys.remove ipc_addr

let _ =
  Shutdown.don't_finish_before @@ main ();
  never_returns @@ Scheduler.go ()
