-module(socks5_SUITE).

-include_lib("kernel/include/inet.hrl").

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).
-compile(nowarn_export_all).

all() ->
  [
    connection_test,
    multiple_connections_test,
    username_password_auth_test,
    wrong_username_password_auth_test,
    proxy_test,
    domain_proxy_test
  ].

%%====================================================================
%% Init_per_* functions
%%====================================================================

init_per_suite(Config) ->
  application:ensure_all_started(erlang_socks5),
  Config.

end_per_suite(_Config) ->
  application:stop(erlang_socks5).

%%====================================================================
%% Tests
%%====================================================================

connection_test(_Config) ->
  Socket = connect_socks5(make_socket()),
  ?assert(is_port(Socket)),
  gen_tcp:close(Socket).

multiple_connections_test(_Config) ->
  Socket0 = connect_socks5(make_socket()),
  ?assert(is_port(Socket0)),
  Socket1 = connect_socks5(make_socket()),
  ?assert(is_port(Socket1)),
  gen_tcp:close(Socket0),
  gen_tcp:close(Socket1).

username_password_auth_test(_Config) ->
  Username = <<"testuser">>,
  Password = <<"testpassword">>,
  Socket = make_authorized_socket(Username, Password),
  ?assert(is_port(Socket)),
  gen_tcp:shutdown(Socket, read_write).

wrong_username_password_auth_test(_Config) ->
  Username = <<"testuser">>,
  Password = <<"wrongpassword">>,
  ErrorResponse = make_authorized_socket(Username, Password),
  ?assertEqual(<<1:8, 16#ff:8>>, ErrorResponse).

proxy_test(_Config) ->
  Username = <<"testuser">>,
  Password = <<"testpassword">>,
  Socket = make_authorized_socket(Username, Password),
  Server = spawn(?MODULE, test_server, [self()]),
  receive {started, Server} -> ok after 2000 -> exit(server_not_started) end,
  Socket0 = make_connect_to_remote_host(Socket, {127, 0, 0, 1}, 10808),
  ?assert(is_port(Socket0)),
  gen_tcp:send(Socket0, <<"echo">>),
  {ok, EchoPacket} = gen_tcp:recv(Socket0, byte_size(<<"echo">>), 2000),
  ?assertEqual(<<"echo">>, EchoPacket),
  gen_tcp:shutdown(Socket, read_write),
  gen_tcp:close(Socket),
  receive {stop, Server} -> ok after 2000 -> exit(server_not_stopped) end.

domain_proxy_test(_Config) ->
  Username = <<"testuser">>,
  Password = <<"testpassword">>,
  Socket = make_authorized_socket(Username, Password),
  Server = spawn(?MODULE, test_server, [self()]),
  receive {started, Server} -> ok after 2000 -> exit(server_not_started) end,
  meck:new(inet, [unstick, passthrough]),
  meck:expect(inet, gethostbyname, fun("www.example.test.com") -> {ok, #hostent{h_addr_list = [{127,0,0,1}]}} end),
  Domain = <<"www.example.test.com">>,
  Socket0 = make_connect_to_remote_host(Socket, Domain, 10808),
  ?assert(is_port(Socket0)),
  gen_tcp:send(Socket0, <<"echo">>),
  {ok, EchoPacket} = gen_tcp:recv(Socket0, byte_size(<<"echo">>), 2000),
  ?assertEqual(<<"echo">>, EchoPacket),
  gen_tcp:shutdown(Socket0, read_write),
  gen_tcp:close(Socket0),
  receive {stop, Server} -> ok after 2000 -> exit(server_not_stopped) end,
  meck:unload(inet).

%%====================================================================
%% Internal functions
%%====================================================================

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
    <<1:8, 0:8>> -> Socket;
    Fail ->
      gen_tcp:close(Socket),
      Fail
  end.

make_connect_to_remote_host(Socket, Domain, Port) when is_binary(Domain) ->
  gen_tcp:send(Socket, <<5:8, 1:8, 0:8, 3:8, (byte_size(Domain)):8, Domain/binary, Port:16>>),
  {ok, ResponsePacket} = gen_tcp:recv(Socket, 10, 2000),
  <<5:8, Rep:8, 0:8, 1:8, DstAddr:32, _:16>> = ResponsePacket,
  ?assertEqual(inet4_octets({127,0,0,1}), DstAddr),
  case Rep of
    0 ->
      Socket
  end;
make_connect_to_remote_host(Socket, Ipv4, Port) when size(Ipv4) == 4 ->
  gen_tcp:send(Socket, <<5:8, 1:8, 0:8, 1:8, (inet4_octets(Ipv4)):32, Port:16>>),
  {ok, RequestPacket} = gen_tcp:recv(Socket, 10, 2000),
  <<5:8, Rep:8, 0:8, 1:8, DstAddr:32, _:16>> = RequestPacket,
  ?assertEqual((inet4_octets(Ipv4)), DstAddr),
  case Rep of
    0 ->
      Socket
  end.

test_server(Pid) ->
  {ok, ListenSocket} = gen_tcp:listen(10808, [
    binary,
    {ifaddr, loopback},
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

inet4_octets({Oct1, Oct2, Oct3, Oct4}) ->
  B1 = Oct1 bsl 24,
  B2 = Oct2 bsl 16,
  B3 = Oct3 bsl 8,
  B4 = Oct4,
  B1 + B2 + B3 + B4.
