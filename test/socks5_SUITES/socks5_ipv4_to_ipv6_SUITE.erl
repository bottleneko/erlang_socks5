-module(socks5_ipv4_to_ipv6_SUITE).

-include_lib("kernel/include/inet.hrl").

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-compile(export_all).
-compile(nowarn_export_all).

all() ->
  [
    proxy_test,
    domain_proxy_test,
    upd_associate_test
  ].

%%====================================================================
%% Init_per_* functions
%%====================================================================

init_per_suite(Config) ->
%%  dbg:tracer(),
%%  dbg:p(all, c),
%%  dbg:tpl(socks5_listener, []),
%%  dbg:tpl(socks5_app, []),
%%  dbg:tpl(socks5_connection_sup, []),
%%  dbg:tpl(socks5_config, []),
  application:ensure_all_started(socks5),
  Config.

init_per_testcase(proxy_test, Config) ->
  Username = <<"testuser">>,
  Password = <<"testpassword">>,
  Socket = socks5_ipv4_helpers:make_authorized_socket(Username, Password),
  ServerProcess = spawn(socks5_ipv6_helpers, test_server, [self()]),
  receive {started, ServerProcess} -> ok after 2000 -> exit(server_not_started) end,
  [{socket, Socket} , {server_process, ServerProcess} | Config];
init_per_testcase(domain_proxy_test, Config) ->
  Username = <<"testuser">>,
  Password = <<"testpassword">>,
  Socket = socks5_ipv4_helpers:make_authorized_socket(Username, Password),
  ServerProcess = spawn(socks5_ipv6_helpers, test_server, [self()]),
  receive {started, ServerProcess} -> ok after 2000 -> exit(server_not_started) end,
  meck:new(inet, [unstick, passthrough]),
  meck:expect(inet, gethostbyname, fun("www.example.test.com") -> {ok, #hostent{h_addr_list = [{0, 0, 0, 0, 0, 0, 0, 1}]}} end),
  [{socket, Socket}, {server_process, ServerProcess} | Config];
init_per_testcase(upd_associate_test, Config) ->
  Username = <<"testuser">>,
  Password = <<"testpassword">>,
  Socket = socks5_ipv4_helpers:make_authorized_socket(Username, Password),
  {ok, SendSocket} = gen_udp:open(0, [{active, false}, binary]),
  {ok, ReceiveSocket} = gen_udp:open(0, [{active, false}, binary, inet6]),
  [{tcp_socket, Socket}, {udp_sockets, {SendSocket, ReceiveSocket}} | Config];

init_per_testcase(_, Config) ->
  Config.

%%====================================================================
%% End_per_* functions
%%====================================================================

end_per_suite(_Config) ->
  application:stop(socks5).

end_per_testcase(proxy_test, Config) ->
  ServerSocket = ?config(socket, Config),
  ServerProcess = ?config(server_process, Config),
  gen_tcp:shutdown(ServerSocket, read_write),
  gen_tcp:close(ServerSocket),
  receive {stop, ServerProcess} -> ok after 2000 -> exit(server_not_stopped) end;
end_per_testcase(domain_proxy_test, Config) ->
  ServerSocket = ?config(socket, Config),
  ServerProcess = ?config(server_process, Config),
  gen_tcp:shutdown(ServerSocket, read_write),
  gen_tcp:close(ServerSocket),
  receive {stop, ServerProcess} -> ok after 2000 -> exit(server_not_stopped) end,
  meck:unload(inet);
end_per_testcase(upd_associate_test, Config) ->
  ServerSocket = ?config(tcp_socket, Config),
  gen_tcp:shutdown(ServerSocket, read_write),
  gen_tcp:close(ServerSocket),

  {SendSocket, ReceiveSocket} = ?config(udp_sockets, Config),
  gen_udp:close(ReceiveSocket),
  gen_udp:close(SendSocket);

end_per_testcase(_, Config) ->
  Config.

%%====================================================================
%% IPv6 tests
%%====================================================================

proxy_test(Config) ->
  ServerSocket = ?config(socket, Config),
  Socket0 = socks5_helpers:make_connect_to_remote_host(ServerSocket, {0, 0, 0, 0, 0, 0, 0, 1}, 10808),
  ?assert(is_port(Socket0)),
  gen_tcp:send(Socket0, <<"echo">>),
  {ok, EchoPacket} = gen_tcp:recv(Socket0, byte_size(<<"echo">>), 2000),
  ?assertEqual(<<"echo">>, EchoPacket).

domain_proxy_test(Config) ->
  ServerSocket = ?config(socket, Config),
  Domain = <<"www.example.test.com">>,
  Socket0 = socks5_helpers:make_connect_to_remote_host(ServerSocket, Domain, 10808),
  ?assert(is_port(Socket0)),
  gen_tcp:send(Socket0, <<"echo">>),
  {ok, EchoPacket} = gen_tcp:recv(Socket0, byte_size(<<"echo">>), 2000),
  ?assertEqual(<<"echo">>, EchoPacket).

upd_associate_test(Config) ->
  TcpSocket = ?config(tcp_socket, Config),
  {SendSocket, ReceiveSocket} = ?config(udp_sockets, Config),

  {ok, Port} = inet:port(ReceiveSocket),
  {BndAddr, BndPort} = socks5_helpers:make_udp_assoc_to_remote_host(TcpSocket, {0, 0, 0, 0, 0, 0, 0, 1}, Port),
  gen_udp:send(SendSocket, BndAddr, BndPort, <<"echo">>),

  {ok, {_, _, EchoPacket}} = gen_udp:recv(ReceiveSocket, byte_size(<<"echo">>)),
  ?assertEqual(<<"echo">>, EchoPacket).