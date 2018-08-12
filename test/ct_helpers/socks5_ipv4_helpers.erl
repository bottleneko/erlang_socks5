-module(socks5_ipv4_helpers).

-include("socks5_method_codes.hrl").

%% API
-export([make_socket/0]).
-export([make_authorized_socket/2]).
-export([test_server/1]).


make_socket() ->
  {ok, Socket} = gen_tcp:connect({127,0,0,1}, 10800, [binary, {packet, raw}, {active, false}]),
  Socket.

make_authorized_socket(Username, Password) ->
  Socket = make_socket(),
  gen_tcp:send(Socket, <<5:8, 1:8, 2:8>>),
  {ok, _MethodPacket} = gen_tcp:recv(Socket, 2, 2000),
  AuthPacket = <<1:8, (byte_size(Username)):8, Username/binary, (byte_size(Password)):8, Password/binary>>,
  gen_tcp:send(Socket, AuthPacket),
  {ok, AuthRespPacket} = gen_tcp:recv(Socket, 2, 2000),
  case AuthRespPacket of
    <<?USERNAME_PASSWORD_AUTH_VERSION, ?USERNAME_PASSWORD_AUTH_SUCCESS>> -> Socket;
    Fail ->
      gen_tcp:close(Socket),
      Fail
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