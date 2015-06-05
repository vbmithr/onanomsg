open Core.Std
open Async.Std
open OUnit
open Nanomsg
open Nanomsg_async

let ipc_addr = "/tmp/onanomsg_async_ipc"

let ths = ref []

let or_fail = function
  | `Error (msg, msg2) -> failwith @@ msg ^ ": " ^ msg2
  | `Ok v -> v

let async_socket () =
  let pub = or_fail @@ socket_wo Pub in
  ignore @@ or_fail @@ bind pub @@ `Ipc ipc_addr

let pair_test () =
  let msgs = ["auie"; "uie,"; "yx.k"] in
  let addr = `Inproc "rdv_point" in
  let peer1 = or_fail @@ socket_rw Pair in
  let peer2 = or_fail @@ socket_rw Pair in
  let _ = or_fail @@ bind peer1 addr in
  let _ = or_fail @@ connect peer2 addr in
  Deferred.List.map ~how:`Sequential ~f:(fun msg ->
      send_string peer1 msg >>= fun maybe_error_send ->
      let (_:int) = or_fail maybe_error_send in
      recv_string peer2 >>| fun maybe_error_recv ->
      assert_equal msg @@ or_fail maybe_error_recv
    ) msgs
  >>= fun _ ->
  (* Deferred.all_unit @@ *)
  (* List.map ~f:(fun msg -> *)
  (*     send_string peer2 msg >>= fun maybe_error_send -> *)
  (*     recv_string peer1 >>| fun maybe_error_recv -> *)
  (*     assert_equal msg @@ or_fail maybe_error_recv *)
  (*   ) msgs *)
  (* >>| fun () -> *)
  or_fail @@ close peer1;
  or_fail @@ close peer2;
  Deferred.unit

let ipc_pubsub_test () =
  let pub = or_fail @@ socket_wo Pub in
  let sub = or_fail @@ socket_ro Sub in
  let _ = bind pub @@ `Ipc ipc_addr in
  let _ = connect sub @@ `Ipc ipc_addr in
  let _ = or_fail @@ with_nn_sock sub (fun nn_sock -> subscribe nn_sock "") in

  let msg = "bleh" in
  let len = String.length msg in
  let recv_msg = Bytes.create @@ String.length msg in
  let recv_msg' = Bytes.create @@ String.length msg in

  (* NB.send_string *)
  let th = send_string pub msg in
  recv_string sub >>= fun str ->
  let str = or_fail str in
  assert_equal true (Deferred.is_determined th);
  assert_equal msg str;

  (* NB.send_string_buf *)
  let th = send_string_buf pub msg 0 len in
  recv_bytes_buf sub recv_msg 0 >>= fun nb_recv ->
  let nb_recv = or_fail nb_recv in
  assert_equal len nb_recv;
  assert_equal true (Deferred.is_determined th);
  assert_equal msg (Bytes.unsafe_to_string recv_msg);

  (* NB.send_bytes *)
  let th = send_bytes pub recv_msg in
  recv_bytes_buf sub recv_msg' 0 >>| fun nb_recv ->
  let nb_recv = or_fail nb_recv in
  assert_equal len nb_recv;
  assert_equal true (Deferred.is_determined th);
  assert_equal recv_msg recv_msg';

  or_fail @@ close sub;
  or_fail @@ close pub

let run_async_test test_f () =
  let th = test_f () in
  ths := th :: !ths

let test_suite =
  [
    "async_socket", `Quick, async_socket;
    "pair", `Quick, run_async_test pair_test;
    "ipc_pubsub", `Quick, run_async_test ipc_pubsub_test;
  ]

let try_remove fn =
  try_with (fun () -> Sys.remove ipc_addr) >>| ignore

let main () =
  try_with
    (fun () ->
       pair_test () >>= fun () ->
       (* ipc_pubsub_test () >>= fun () -> *)
       try_remove ipc_addr
    ) >>= function
  | Ok () -> Shutdown.exit 0
  | Error exn ->
    try_remove ipc_addr >>= fun () ->
    raise exn

let _ =
  don't_wait_for @@ main ();
  never_returns @@ Scheduler.go ()
