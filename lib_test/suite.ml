open OUnit

open Nanomsg

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

let common_sock sock =
  let open CCError in
  get_linger sock >>= fun sock_linger ->
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
  close sock

let socket_ro_test () =
  let domains = [AF_SP; AF_SP_RAW] in
  let protos_ro = [Sub; Pull] in
  List.iter
    (fun d ->
       List.iter
         (fun p ->
            let open CCError in
            get_exn
              (socket_ro ~domain:d p >>= fun sock ->
               domain sock >>= fun sock_domain ->
               proto_int sock >>= fun sock_proto_int ->
               assert_equal d sock_domain;
               assert_equal (proto_ro_to_enum p) sock_proto_int;
               common_sock sock
              )
         ) protos_ro
    )
    domains

let socket_wo_test () =
  let domains = [AF_SP; AF_SP_RAW] in
  let protos_wo = [Pub; Push] in
  List.iter
    (fun d ->
       List.iter
         (fun p ->
            let open CCError in
            get_exn
              (socket_wo ~domain:d p >>= fun sock ->
               domain sock >>= fun sock_domain ->
               proto_int sock >>= fun sock_proto_int ->
               assert_equal d sock_domain;
               assert_equal (proto_wo_to_enum p) sock_proto_int;
               common_sock sock
              )
         ) protos_wo
    )
    domains

let socket_rw_test () =
  let domains = [AF_SP; AF_SP_RAW] in
  let protos_rw = [Pair; Req; Rep; Surveyor; Respondant; Bus] in
  List.iter
    (fun d ->
       List.iter
         (fun p ->
            let open CCError in
            get_exn
              (socket_rw ~domain:d p >>= fun sock ->
               domain sock >>= fun sock_domain ->
               proto_int sock >>= fun sock_proto_int ->
               assert_equal d sock_domain;
               assert_equal (proto_rw_to_enum p) sock_proto_int;
               common_sock sock
              )
         ) protos_rw
    )
    domains

let send_recv_fd_test () =
  let open CCError in
  let sock = get_exn @@ socket_rw Pair in
  ignore @@ recv_fd sock;
  ignore @@ send_fd sock;
  get_exn @@ close sock


let reqrep_test () =
  let open CCError in
  let receiver = get_exn @@ socket_rw Rep in
  let sender = get_exn @@ socket_rw Req in
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
  socket_ro Sub >>= fun sub ->
  subscribe sub "" >>= fun () ->
  connect sub address >>= fun _ ->
  let packet = "foo bar baz" in
  socket_wo Pub >>= fun pub ->
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
  socket_ro Sub >>= fun sub1 ->
  socket_ro Sub >>= fun sub2 ->
  let _ = connect sub1 addr1 in
  let _ = connect sub2 addr2 in
  subscribe sub1 "" >>= fun () ->
  subscribe sub2 "" >>= fun () ->
  let packet = "one two three" in
  socket_wo Pub >>= fun pub ->
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

let or_fail = function
  | `Ok v -> v
  | `Error (a, b) ->
    raise (Failure (Printf.sprintf "%s: %s" a b))

let pipeline_local_test () =
  let open CCError in
  (* let m = Mutex.create () in *)
  (* let c = Condition.create () in *)
  let addr = `Inproc "rdvpoint" in
  let msgs = ["foo"; "bar"; "baz"] in
  let receiver addr =
    socket_ro Pull >>= fun s ->
    bind s addr >>= fun _ ->
    map_err (fun errs -> List.hd errs) @@
    choose @@
    List.map
      (fun msg ->
         recv_string s >|= fun m ->
         assert_equal
           ~msg:"pipeline_local_test"
           ~printer:(fun i -> i) msg m
      ) msgs
    (* >|= fun _ -> *)
    (* Condition.signal c; *)
    (* Mutex.unlock m *)
  in
  let sender addr =
    socket_wo Push >>= fun s ->
    connect s addr >|= fun _ ->
    List.iter (fun msg -> get_exn @@ send_string s msg) msgs;
    (* Mutex.lock m; *)
    (* Condition.wait c m; *)
    close s
  in
  let t1 = Thread.create receiver addr in
  let t2 = Thread.create sender addr in
  ()
  (* Thread.join t1; *)
  (* Thread.join t2 *)

let test_suite =
  [
    "bind_addr", `Quick, bind_addr_test;
    "connect_of_string", `Quick, connect_of_string_test;
    "connect_to_string", `Quick, connect_to_string_test;
    "socket_ro", `Quick, socket_ro_test;
    "socket_wo", `Quick, socket_wo_test;
    "socket_rw", `Quick, socket_rw_test;
    "send_recv_fd", `Quick, send_recv_fd_test;
    "reqrep", `Quick, (fun () -> CCError.get_exn @@ reqrep_test ());
    "pubsub_local", `Quick, (fun () -> CCError.get_exn @@ pubsub_local_test ());
    "pubsub_local_2subs", `Quick, (fun () -> CCError.get_exn @@ pubsub_local_2subs_test ());
    "pipeline_local", `Quick, pipeline_local_test;
  ]

let () =
  Alcotest.run "ONanomsg test suite" [
    "test_suite", test_suite;
  ];
