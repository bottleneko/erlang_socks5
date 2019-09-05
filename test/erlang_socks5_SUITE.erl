-module(erlang_socks5_SUITE).

-define(DEFAULT_USERNAME, <<"testuser">>).
-define(DEFAULT_PASSWORD, <<"testpassword">>).
-define(DEFAULT_DOMAIN, <<"www.example.test.com">>).

-include("socks5_method_codes.hrl").

-include_lib("kernel/include/inet.hrl").

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).
-compile(nowarn_missing_spec).

%%====================================================================
%% Common Test callbacks
%%====================================================================

all() ->
  [connection_test,
   multiple_connections_test,
   username_password_auth_test,
   wrong_username_password_auth_test,
   proxy_test,
   domain_proxy_test,
   upd_associate_test
  ].

init_per_suite(Config) ->
  application:ensure_all_started(erlang_socks5),
  Config.

init_per_testcase(proxy_test, Config) ->
  Socket = make_authorized_socket(?DEFAULT_USERNAME, ?DEFAULT_PASSWORD),
  ServerProcess = run_server_process(),
  [{socket, Socket}, {server_process, ServerProcess} | Config];
init_per_testcase(domain_proxy_test, Config) ->
  Socket = make_authorized_socket(?DEFAULT_USERNAME, ?DEFAULT_PASSWORD),
  ServerProcess = run_server_process(),

  meck:new(inet, [unstick, passthrough]),
  MeckExpectFun =
    fun("www.example.test.com") ->
        {ok, #hostent{h_addr_list = [{127,0,0,1}]}}
    end,
  meck:expect(inet, gethostbyname, MeckExpectFun),

  [{socket, Socket}, {server_process, ServerProcess} | Config];
init_per_testcase(upd_associate_test, Config) ->
  Socket = make_authorized_socket(?DEFAULT_USERNAME, ?DEFAULT_PASSWORD),
  [{socket, Socket} | Config];
init_per_testcase(_, Config) ->
  Config.

end_per_testcase(proxy_test, Config) ->
  ServerSocket = ?config(socket, Config),
  ServerProcess = ?config(server_process, Config),

  stop_server_process(ServerSocket, ServerProcess);
end_per_testcase(domain_proxy_test, Config) ->
  ServerSocket = ?config(socket, Config),
  ServerProcess = ?config(server_process, Config),

  stop_server_process(ServerSocket, ServerProcess),

  meck:unload(inet);
end_per_testcase(upd_associate_test, Config) ->
  ServerSocket = ?config(socket, Config),

  gen_tcp:shutdown(ServerSocket, read_write),
  gen_tcp:close(ServerSocket);
end_per_testcase(_, Config) ->
  Config.

end_per_suite(_Config) ->
  application:stop(erlang_socks5).

%%====================================================================
%% Tests
%%====================================================================

%% @doc Server listen and accept new connections.
connection_test(_Config) ->
  Socket = connect_socks5(make_socket()),
  ?assert(is_port(Socket)),

  gen_tcp:close(Socket).

%% @doc Server listen and accept several new connections.
multiple_connections_test(_Config) ->
  Socket0 = connect_socks5(make_socket()),
  ?assert(is_port(Socket0)),
  Socket1 = connect_socks5(make_socket()),
  ?assert(is_port(Socket1)),
  gen_tcp:close(Socket0),
  gen_tcp:close(Socket1).

%% @doc Server listen and accept new connections with authentication
username_password_auth_test(_Config) ->
  Socket = make_authorized_socket(?DEFAULT_USERNAME, ?DEFAULT_PASSWORD),
  ?assert(is_port(Socket)),
  gen_tcp:shutdown(Socket, read_write).

%% @doc Server not accept connections with wrong login or/and password
wrong_username_password_auth_test(_Config) ->
  Username = <<"wronglogin">>,
  Password = <<"wrongpassword">>,
  ?assertEqual(<<1:8, ?USERNAME_PASSWORD_AUTH_FAIL:8>>, make_authorized_socket(?DEFAULT_USERNAME, Password)),
  ?assertEqual(<<1:8, ?USERNAME_PASSWORD_AUTH_FAIL:8>>, make_authorized_socket(Username, ?DEFAULT_PASSWORD)),
  ?assertEqual(<<1:8, ?USERNAME_PASSWORD_AUTH_FAIL:8>>, make_authorized_socket(Username, Password)).

%% @doc Server will be proxying data to local host by address
proxy_test(Config) ->
  ServerSocket = ?config(socket, Config),
  Socket0 = make_connect_to_remote_host(ServerSocket, {127, 0, 0, 1}, 10808),

  gen_tcp:send(Socket0, <<"echo">>),
  {ok, EchoPacket} = gen_tcp:recv(Socket0, byte_size(<<"echo">>), 2000),
  ?assertEqual(<<"echo">>, EchoPacket).

%% @doc Server will be proxying data to local host by domain name
domain_proxy_test(Config) ->
  ServerSocket = ?config(socket, Config),

  Socket0 = make_connect_to_remote_host(ServerSocket, ?DEFAULT_DOMAIN, 10808),
  gen_tcp:send(Socket0, <<"echo">>),
  {ok, EchoPacket} = gen_tcp:recv(Socket0, byte_size(<<"echo">>), 2000),
  ?assertEqual(<<"echo">>, EchoPacket).

%% @doc Server will be proxying UDP data to local host
upd_associate_test(Config) ->
  Socket = ?config(socket, Config),

  {ok, InteractSocket} = gen_udp:open(0, [{active, false}, binary]),
  {ok, Port} = inet:port(InteractSocket),
  {BndAddr, BndPort} = make_udp_assoc_to_remote_host(Socket, {127, 0, 0, 1}, Port),
  gen_udp:send(InteractSocket, BndAddr, BndPort, <<"echo">>),
  {ok, {_, _, EchoPacket}} = gen_udp:recv(InteractSocket, byte_size(<<"echo">>)),
  ?assertEqual(<<"echo">>, EchoPacket),

  gen_udp:close(InteractSocket).

%%====================================================================
%% Internal functions
%%====================================================================

run_server_process() ->
  ServerProcess = spawn(?MODULE, test_server, [self()]),
  receive
    {started, ServerProcess} -> ok
  after 2000 ->
      exit(server_not_started)
  end,
  ServerProcess.

stop_server_process(ServerSocket, ServerProcess) ->
  gen_tcp:shutdown(ServerSocket, read_write),
  gen_tcp:close(ServerSocket),

  receive
    {stop, ServerProcess} -> ok
  after 2000 ->
      exit(server_not_stopped)
  end.

make_socket() ->
  {ok, Socket} = gen_tcp:connect({127,0,0,1}, 10800, [binary, {packet, raw}, {active, false}]),
  Socket.

connect_socks5(Socket) ->
  gen_tcp:send(Socket, <<5:8, 1:8, 2:8>>),
  {ok, Packet} = gen_tcp:recv(Socket, 2, 2000),
  case Packet of
    <<5:8, 2:8>> -> Socket;
    Fail ->
      gen_tcp:close(Socket),
      Fail
  end.

make_authorized_socket(Username, Password) ->
  Socket = make_socket(),
  gen_tcp:send(Socket, <<5:8, 1:8, 2:8>>),
  {ok, MethodPacket} = gen_tcp:recv(Socket, 2, 2000),
  ?assertEqual(<<5:8, 2:8>>, MethodPacket),
  AuthPacket = <<1:8, (byte_size(Username)):8, Username/binary, (byte_size(Password)):8, Password/binary>>,
  gen_tcp:send(Socket, AuthPacket),
  {ok, AuthRespPacket} = gen_tcp:recv(Socket, 2, 2000),
  case AuthRespPacket of
    <<?USERNAME_PASSWORD_AUTH_VERSION, ?USERNAME_PASSWORD_AUTH_SUCCESS>> -> Socket;
    Fail ->
      gen_tcp:close(Socket),
      Fail
  end.

make_connect_to_remote_host(Socket, Domain, Port) when is_binary(Domain) ->
  gen_tcp:send(Socket,
    <<?PROTOCOL_VERSION, ?CONNECT_CMD, ?RESERVED, ?QUERY_FQDN, (byte_size(Domain)), Domain/binary, Port:16>>),
  {ok, ResponsePacket} = gen_tcp:recv(Socket, 10, 2000),
  <<?PROTOCOL_VERSION, Rep, ?RESERVED, ?QUERY_IPv4, DstAddr:32, _:16>> = ResponsePacket,
  ?assertEqual(inet4_number({127,0,0,1}), DstAddr),
  case Rep of
    0 ->
      Socket
  end;
make_connect_to_remote_host(Socket, Ipv4, Port) when size(Ipv4) == 4 ->
  gen_tcp:send(Socket,
    <<?PROTOCOL_VERSION, ?CONNECT_CMD, ?RESERVED, ?QUERY_IPv4, (inet4_number(Ipv4)):32, Port:16>>),
  {ok, RequestPacket} = gen_tcp:recv(Socket, 10, 2000),
  <<?PROTOCOL_VERSION, Rep, ?RESERVED, ?QUERY_IPv4, DstAddr:32, _:16>> = RequestPacket,
  ?assertEqual(inet4_number({127,0,0,1}), DstAddr),
  case Rep of
    0 ->
      Socket
  end;
make_connect_to_remote_host(Socket, Ipv6, Port) when size(Ipv6) == 6 ->
  gen_tcp:send(Socket,
    <<?PROTOCOL_VERSION, ?CONNECT_CMD, ?RESERVED, ?QUERY_IPv6, (inet4_number(Ipv6)):32, Port:16>>),
  {ok, RequestPacket} = gen_tcp:recv(Socket, 10, 2000),
  <<?PROTOCOL_VERSION, Rep, ?RESERVED, ?QUERY_IPv6, DstAddr:32, _:16>> = RequestPacket,
  ?assertEqual(inet6_number({0,0,0,0,0,0,0,1}), DstAddr),
  case Rep of
    0 ->
      Socket
  end.

make_udp_assoc_to_remote_host(Socket, Ipv4, Port) when size(Ipv4) == 4 ->
  gen_tcp:send(Socket,
    <<?PROTOCOL_VERSION, ?UDP_ASSOC_CMD, ?RESERVED, ?QUERY_IPv4, (inet4_number(Ipv4)):32, Port:16>>),
  {ok, ResponsePacket} = gen_tcp:recv(Socket, 10, 2000),
  <<?PROTOCOL_VERSION, Rep, ?RESERVED, ?QUERY_IPv4, DstAddr:32, DstPort:16>> = ResponsePacket,
  ?assertEqual(inet4_number({127,0,0,1}), DstAddr),
  case Rep of
    0 ->
      {{127,0,0,1}, DstPort}
  end.

test_server(Pid) ->
  {ok, ListenSocket} = gen_tcp:listen(10808, [
    binary,
    {ifaddr, {127,0,0,1}},
    {packet, raw},
    {active, true},
    {reuseaddr, true}
  ]),
  Pid ! {started, self()},
  {ok, Client} = gen_tcp:accept(ListenSocket),
  receive
    {tcp, Client, Data} ->
      gen_tcp:send(Client, Data)
  end,
  gen_tcp:shutdown(Client, read_write),
  gen_tcp:shutdown(ListenSocket, read_write),
  gen_tcp:close(Client),
  gen_tcp:close(ListenSocket),
  Pid ! {stop, self()}.

inet4_number({Oct1, Oct2, Oct3, Oct4}) ->
  B1 = Oct1 bsl 24,
  B2 = Oct2 bsl 16,
  B3 = Oct3 bsl 8,
  B4 = Oct4,
  B1 + B2 + B3 + B4.

inet6_number({Hex1, Hex2, Hex3, Hex4, Hex5, Hex6, Hex5, Hex6}) ->
  B1 = Hex1 bsl 112,
  B2 = Hex2 bsl 96,
  B3 = Hex3 bsl 80,
  B4 = Hex4 bsl 64,
  B5 = Hex4 bsl 48,
  B7 = Hex4 bsl 32,
  B6 = Hex4 bsl 16,
  B8 = Hex4,
  B1 + B2 + B3 + B4 + B5 + B6 + B7 + B8.
