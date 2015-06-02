open Lwt.Infix
open OUnit

open Nanomsg
module NB = Nanomsg_lwt

let ths = ref []

let bind_addr_test () =
  let open Addr in
  assert_equal (`Inproc "9786/+-auieauie7658%=`!!")
    (bind_of_string "inproc://9786/+-auieauie7658%=`!!");
  assert_equal (`Ipc "9786/+-auieauie7658%=`!!")
    (bind_of_string "ipc://9786/+-auieauie7658%=`!!");
  assert_equal (`Tcp (`All,  1234))
    (bind_of_string "tcp://*:1234");
  assert_equal (`Tcp (`V4 Ipaddr.V4.localhost, 1234))
    (bind_of_string "tcp://127.0.0.1:1234");
  assert_equal ~msg:"ipv6" (`Tcp (`V6 Ipaddr.V6.localhost, 1234))
    (bind_of_string "tcp://::1:1234");
  assert_equal ~msg:"ifname" (`Tcp (`Iface "eth0", 1234))
    (bind_of_string "tcp://eth0:1234")

let connect_of_string_test () =
  let open Addr in
  assert_equal (`Inproc "9786/+-auieauie7658%=`!!")
    (connect_of_string "inproc://9786/+-auieauie7658%=`!!");
  assert_equal (`Ipc "9786/+-auieauie7658%=`!!")
    (connect_of_string "ipc://9786/+-auieauie7658%=`!!");
  assert_equal
    ~printer:(Addr.show Addr.pp_connect) ~msg:"tcp_with_iface"
    (`Tcp ((`V4 Ipaddr.V4.localhost, Some (`Iface "eth0")), 1234))
    (connect_of_string "tcp://eth0;127.0.0.1:1234");
  assert_equal
    ~printer:(Addr.show Addr.pp_connect) ~msg:"tcp_without_iface"
    (`Tcp ((`V4 Ipaddr.V4.localhost, None), 1234))
    (connect_of_string "tcp://127.0.0.1:1234");
  assert_equal
    ~printer:(Addr.show Addr.pp_connect) ~msg:"dns"
    (`Tcp ((`Dns "localhost", None), 1234))
    (connect_of_string "tcp://localhost:1234");
  assert_equal
    ~printer:(Addr.show Addr.pp_connect) ~msg:"::1_none"
    (`Tcp ((`V6 (Ipaddr.V6.localhost), None), 1234))
    (connect_of_string "tcp://::1:1234");
  assert_equal
    ~printer:(Addr.show Addr.pp_connect) ~msg:"dns_with_iface"
    (`Tcp ((`Dns "localhost", Some (`Iface "lo0")), 1234))
    (connect_of_string "tcp://lo0;localhost:1234");
  assert_equal
    ~printer:(Addr.show Addr.pp_connect) ~msg:"ipv6_iface_with_ipv6_addr"
    (`Tcp ((`V6 (Ipaddr.V6.of_string_exn "dead::beef"), Some (`V6 Ipaddr.V6.localhost)), 1234))
    (connect_of_string "tcp://::1;dead::beef:1234");
  assert_equal
    ~printer:(Addr.show Addr.pp_connect) ~msg:"ipv6_addr_wo_iface"
    (`Tcp ((`V6 (Ipaddr.V6.of_string_exn "dead::beef"), None), 1234))
    (connect_of_string "tcp://dead::beef:1234")

let connect_to_string_test () =
  let open Addr in
  assert_equal
    (connect_of_string "inproc://9786/+-auieauie7658%=`!!")
    (`Inproc "9786/+-auieauie7658%=`!!");
  assert_equal
    (connect_of_string "ipc://9786/+-auieauie7658%=`!!")
    (`Ipc "9786/+-auieauie7658%=`!!");
  assert_equal
    ~printer:(Addr.show Addr.pp_connect) ~msg:"tcp_with_iface"
    (connect_of_string "tcp://eth0;127.0.0.1:1234")
    (`Tcp ((`V4 Ipaddr.V4.localhost, Some (`Iface "eth0")), 1234));
  assert_equal
    ~printer:(Addr.show Addr.pp_connect) ~msg:"tcp_without_iface"
    (connect_of_string "tcp://127.0.0.1:1234")
    (`Tcp ((`V4 Ipaddr.V4.localhost, None), 1234));
  assert_equal
    ~printer:(Addr.show Addr.pp_connect) ~msg:"dns"
    (connect_of_string "tcp://localhost:1234")
    (`Tcp ((`Dns "localhost", None), 1234));
  assert_equal
    ~printer:(Addr.show Addr.pp_connect) ~msg:"::1_none"
    (connect_of_string "tcp://::1:1234")
    (`Tcp ((`V6 (Ipaddr.V6.localhost), None), 1234));
  assert_equal
    ~printer:(Addr.show Addr.pp_connect) ~msg:"dns_with_iface"
    (connect_of_string "tcp://lo0;localhost:1234")
    (`Tcp ((`Dns "localhost", Some (`Iface "lo0")), 1234));
  assert_equal
    ~printer:(Addr.show Addr.pp_connect) ~msg:"ipv6_iface_with_ipv6_addr"
    (connect_of_string "tcp://::1;dead::beef:1234")
    (`Tcp ((`V6 (Ipaddr.V6.of_string_exn "dead::beef"), Some (`V6 Ipaddr.V6.localhost)), 1234));
  assert_equal
    ~printer:(Addr.show Addr.pp_connect) ~msg:"ipv6_addr_wo_iface"
    (connect_of_string "tcp://dead::beef:1234")
    (`Tcp ((`V6 (Ipaddr.V6.of_string_exn "dead::beef"), None), 1234))

let socket_test () =
  let domains = [AF_SP; AF_SP_RAW] in
  let protos = [Pair; Pub; Sub; Req; Rep; Push; Pull; Surveyor; Respondant; Bus] in
  List.iter
    (fun d ->
       List.iter
         (fun p ->
            let open CCError in
            get_exn
              (socket ~domain:d p >>= fun sock ->
               domain sock >>= fun sock_domain ->
               proto sock >>= fun sock_proto ->
               get_linger sock >>= fun sock_linger ->
               assert_equal d sock_domain;
               assert_equal p sock_proto;
               assert_equal (`Ms 1000) sock_linger;
               set_linger sock `Inf >>= fun () ->
               get_linger sock >>= fun sock_linger ->
               assert_equal `Inf sock_linger;
               set_send_bufsize sock 256 >>= fun () ->
               set_recv_bufsize sock 256 >>= fun () ->
               get_send_bufsize sock >>= fun send_bufsize ->
               get_recv_bufsize sock >>= fun recv_bufsize ->
               assert_equal 256 send_bufsize;
               assert_equal 256 recv_bufsize;
               close sock)
         )
         protos
    )
    domains

let send_recv_fd_test () =
  let open CCError in
  let sock = get_exn @@ socket Pair in
  ignore @@ recv_fd sock;
  ignore @@ send_fd sock;
  get_exn @@ close sock

let pair_test () =
  let module E = CCError in
  let msgs = ["auie"; "uie,"; "yx.k"] in
  let addr = `Inproc "rdv_point" in
  let peer1 = E.get_exn @@ socket Pair in
  let peer2 = E.get_exn @@ socket Pair in
  let _ = bind peer1 addr in
  let _ = connect peer2 addr in
  Lwt_list.iter_s (fun msg ->
      NB.send_string peer1 msg >>= fun () ->
      NB.recv_string peer2 >>= fun recv_msg ->
      assert_equal msg recv_msg; Lwt.return_unit
    ) msgs >>= fun () ->
  Lwt_list.iter_s (fun msg ->
      NB.send_string peer2 msg >>= fun () ->
      NB.recv_string peer1 >>= fun recv_msg ->
      assert_equal msg recv_msg; Lwt.return_unit
    ) msgs >|= fun () ->
  E.get_exn @@ close peer1;
  E.get_exn @@ close peer2

let reqrep_test () =
  let open CCError in
  let receiver = get_exn @@ socket Rep in
  let sender = get_exn @@ socket Req in
  let _ = bind receiver @@ `Inproc "*" in
  let _ = connect sender @@ `Inproc "*" in
  let packet = "testing" in
  send_string sender packet >>= fun () ->
  recv_string receiver >>= fun received ->
  close receiver >>= fun () ->
  close sender >|= fun () ->
  assert_equal packet received

let pubsub_local_test () =
  let open CCError in
  let address = `Inproc "t2" in
  socket Sub >>= fun sub ->
  subscribe sub "" >>= fun () ->
  connect sub address >>= fun _ ->
  let packet = "foo bar baz" in
  socket Pub >>= fun pub ->
  bind pub address >>= fun _ ->
  send_string pub packet >>= fun _ ->
  recv_string sub >>= fun recv_msg ->
  close pub >>= fun _ ->
  close sub >|= fun _ ->
  assert_equal packet recv_msg

let pubsub_local_2subs_test () =
  let open CCError in
  let addr1 = `Inproc "tt1" in
  let addr2 = `Inproc "tt2" in
  socket Sub >>= fun sub1 ->
  socket Sub >>= fun sub2 ->
  let _ = connect sub1 addr1 in
  let _ = connect sub2 addr2 in
  subscribe sub1 "" >>= fun () ->
  subscribe sub2 "" >>= fun () ->
  let packet = "one two three" in
  socket Pub >>= fun pub ->
  let _ = bind pub addr1 in
  let _ = bind pub addr2 in
  send_string pub packet >>= fun () ->
  recv_string sub1 >>= fun x1 ->
  recv_string sub2 >>= fun x2 ->
  close pub >>= fun () ->
  close sub1 >>= fun () ->
  close sub2 >|= fun () ->
  assert_equal packet x1;
  assert_equal packet x2

let tcp_pubsub_test () =
  let open Nanomsg_lwt in
  let port = 56352 in
  wrap_error @@ socket Pub >>= fun pub ->
  wrap_error @@ socket Sub >>= fun sub ->
  wrap_error @@ set_ipv4_only pub false >>= fun () ->
  wrap_error @@ set_ipv4_only sub false >>= fun () ->
  let _ = bind pub @@ `Tcp (`All, port) in
  let _ = connect sub @@ `Tcp ((`V6 Ipaddr.V6.localhost, None), port) in
  wrap_error @@ Nanomsg.subscribe sub "" >>= fun () ->
  let msg = "bleh" in
  let len = String.length msg in
  let recv_msg = Bytes.create @@ String.length msg in
  let recv_msg' = Bytes.create @@ String.length msg in
  let open Lwt.Infix in

  (* NB.send_string *)
  let th = NB.send_string pub msg in
  NB.recv_string sub >>= fun str ->
  assert_equal (Lwt.Return ()) (Lwt.state th);
  assert_equal msg str;

  (* NB.send_string_buf *)
  let th = NB.send_string_buf pub msg 0 len in
  NB.recv_bytes_buf sub recv_msg 0 >>= fun nb_recv ->
  assert_equal nb_recv len;
  assert_equal (Lwt.Return ()) (Lwt.state th);
  assert_equal msg (Bytes.unsafe_to_string recv_msg);

  (* NB.send_bytes *)
  let th = NB.send_bytes pub recv_msg in
  NB.recv_bytes_buf sub recv_msg' 0 >|= fun nb_recv ->
  assert_equal nb_recv len;
  assert_equal (Lwt.Return ()) (Lwt.state th);
  assert_equal recv_msg recv_msg';

  CCError.get_exn @@ close pub;
  CCError.get_exn @@ close sub

let pipeline_local_test () =
  let open Nanomsg_lwt in
  let msgs = [|"foo"; "bar"; "baz"|] in
  let receiver addr =
    wrap_error @@ socket Pull >>= fun s ->
    wrap_error @@ bind s addr >>= fun _ ->
    let rec inner n =
      if n > 2
      then wrap_error @@ close s
      else
        NB.recv_string s >>= fun m ->
        (assert_equal msgs.(n) m; inner (succ n))
    in
    inner 0
  in
  let sender addr =
    wrap_error @@ socket Push >>= fun s ->
    wrap_error @@ connect s addr >>= fun _ ->
    Lwt_list.iter_s (NB.send_string s) @@ Array.to_list msgs >>= fun () ->
    Lwt_unix.yield () >>= fun () -> wrap_error @@ close s
  in
  Lwt.join
    [
      sender (`Inproc "rdvpoint");
      receiver (`Inproc "rdvpoint")
    ]

let run_lwt_test test_f () =
  let th = test_f () in
  ths := th :: !ths

let test_suite =
  [
    "bind_addr", `Quick, bind_addr_test;
    "connect_of_string", `Quick, connect_of_string_test;
    "connect_to_string", `Quick, connect_to_string_test;
    "socket", `Quick, socket_test;
    "send_recv_fd", `Quick, send_recv_fd_test;
    "pair", `Quick, run_lwt_test pair_test;
    "reqrep", `Quick, (fun () -> CCError.get_exn @@ reqrep_test ());
    "pubsub_local", `Quick, (fun () -> CCError.get_exn @@ pubsub_local_test ());
    "pubsub_local_2subs", `Quick, (fun () -> CCError.get_exn @@ pubsub_local_2subs_test ());
    "tcp_pubsub", `Quick, run_lwt_test tcp_pubsub_test;
    "pipeline_local", `Quick, run_lwt_test pipeline_local_test;
  ]

let () =
  Alcotest.run "ONanomsg test suite" [
    "test_suite", test_suite;
  ];
  Lwt_main.run @@ Lwt.join !ths
