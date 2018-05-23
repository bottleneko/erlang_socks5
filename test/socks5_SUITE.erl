-module(socks5_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).
-compile(nowarn_export_all).

all() ->
  [
    connection_test,
    multiple_connections_test,
    username_password_auth_test,
    %% wrong_username_password_auth_test,
    proxy_test
  ].

init_per_suite(Config) ->
  application:ensure_all_started(erlang_socks5),
  Config.

end_per_suite(_Config) ->
  application:stop(erlang_socks5).

connection_test(_Config) ->
  {ok, Socket} = gen_tcp:connect({127,0,0,1}, 10800, [binary, {packet, raw}, {active, false}]),
  gen_tcp:send(Socket, <<5:8, 1:8, 2:8>>),
  {ok, Packet} = gen_tcp:recv(Socket, 2, 2000),
  ?assertEqual(<<5:8, 2:8>>, Packet),
  gen_tcp:shutdown(Socket, read_write).

multiple_connections_test(_Config) ->
  {ok, Socket0} = gen_tcp:connect({127,0,0,1}, 10800, [binary, {packet, raw}, {active, false}]),
  gen_tcp:send(Socket0, <<5:8, 1:8, 2:8>>),
  {ok, Socket1} = gen_tcp:connect({127,0,0,1}, 10800, [binary, {packet, raw}, {active, false}]),
  gen_tcp:send(Socket1, <<5:8, 1:8, 2:8>>),
  {ok, Packet0} = gen_tcp:recv(Socket0, 2, 2000),
  {ok, Packet1} = gen_tcp:recv(Socket1, 2, 2000),
  ?assertEqual(<<5:8, 2:8>>, Packet0),
  ?assertEqual(<<5:8, 2:8>>, Packet1),
  gen_tcp:shutdown(Socket0, read_write),
  gen_tcp:shutdown(Socket1, read_write).

username_password_auth_test(_Config) ->
  {ok, Socket} = gen_tcp:connect({127,0,0,1}, 10800, [binary, {packet, raw}, {active, false}]),
  gen_tcp:send(Socket, <<5:8, 1:8, 2:8>>),
  {ok, MethodPacket} = gen_tcp:recv(Socket, 2, 2000),
  ?assertEqual(<<5:8, 2:8>>, MethodPacket),
  Username = <<"testuser">>,
  Password = <<"testpassword">>,
  AuthPacket = <<1:8, (byte_size(Username)):8, Username/binary, (byte_size(Password)):8, Password/binary>>,
  gen_tcp:send(Socket, AuthPacket),
  {ok, AuthRespPacket} = gen_tcp:recv(Socket, 2, 2000),
  ?assertEqual(<<1:8, 0:8>>, AuthRespPacket),
  gen_tcp:shutdown(Socket, read_write).

proxy_test(_Config) ->
  {ok, Socket} = gen_tcp:connect({127,0,0,1}, 10800, [binary, {packet, raw}, {active, false}]),
  gen_tcp:send(Socket, <<5:8, 1:8, 2:8>>),
  {ok, MethodPacket} = gen_tcp:recv(Socket, 2, 2000),
  ?assertEqual(<<5:8, 2:8>>, MethodPacket),
  Username = <<"testuser">>,
  Password = <<"testpassword">>,
  AuthPacket = <<1:8, (byte_size(Username)):8, Username/binary, (byte_size(Password)):8, Password/binary>>,
  gen_tcp:send(Socket, AuthPacket),
  {ok, AuthRespPacket} = gen_tcp:recv(Socket, 2, 2000),
  ?assertEqual(<<1:8, 0:8>>, AuthRespPacket),
  Server = spawn(?MODULE, test_server, [self()]),
  receive {started, Server} -> ok after 2000 -> exit(server_not_started) end,
  gen_tcp:send(Socket, <<5:8, 1:8, 0:8, 1:8, 127:8, 0:8, 0:8, 1:8, 10808:16>>),
  {ok, RequestPacket} = gen_tcp:recv(Socket, 10, 2000),
  %% TODO: check port too
  ?assertMatch(<<5:8, 0:8, 0:8, 1:8, 127:8, 0:8, 0:8, 1:8, _/binary>>, RequestPacket),
  gen_tcp:send(Socket, <<"echo">>),
  {ok, EchoPacket} = gen_tcp:recv(Socket, byte_size(<<"echo">>), 2000),
  ?assertEqual(<<"echo">>, EchoPacket),
  gen_tcp:shutdown(Socket, read_write).

test_server(Pid) ->
  {ok, ListenSocket} = gen_tcp:listen(10808, [
    binary,
    {ifaddr, loopback},
    {packet, raw},
    {active, true}
  ]),
  Pid ! {started, self()},
  {ok, Client} = gen_tcp:accept(ListenSocket),
  receive
    {tcp, Client, Data} ->
      gen_tcp:send(Client, Data)
  end,
  gen_tcp:shutdown(Client, read_write),
  gen_tcp:shutdown(ListenSocket, read_write).