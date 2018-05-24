-module(socks5_statem).
-behaviour(gen_statem).

-include_lib("kernel/include/inet.hrl").

%% API
-export([start_link/2]).

%% gen_statem callbacks
-export([
  init/1,
  format_status/2,
  handle_event/4,
  terminate/3,
  code_change/4,
  callback_mode/0
]).

-export([
  wait_for_socket_control/3,
  wait_auth_methods/3,
  wait_authentication/3,
  wait_socks_request/3,
  data_exchange/3
]).

-define(SERVER, ?MODULE).

-record(state, {
  socket,
  server_socket,
  if_addr,
  username,
  password
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Addr, Socket) ->
  gen_statem:start_link(?MODULE, [Addr, Socket], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

init([Addr, Socket]) ->
  {ok, wait_for_socket_control, #state{ socket = Socket, if_addr = Addr }}.

format_status(_Opt, [_PDict, StateName, _State]) ->
  Status = StateName,
  Status.

wait_for_socket_control(_EventType, socket_delegated, State) ->
  inet:setopts(State#state.socket, [{active, true}]),
  {next_state, wait_auth_methods, State}.

%% rfc 1928
wait_auth_methods(_EventType, {tcp, _Socket, _Data}, State) ->
  gen_tcp:send(State#state.socket, <<5:8, 2:8>>),
  {next_state, wait_authentication, State};
wait_auth_methods(_EventType, {tcp_closed, Socket}, _State) ->
  io:format("SOCKET ~p CLOSED", [Socket]),
  gen_tcp:shutdown(Socket, read_write),
  stop.


%% rfc 1929
wait_authentication(_EventType, {tcp, _Socket, Data}, State) ->
  <<1:8, ULen:8, Username:ULen/binary, PLen:8, Password:PLen/binary>> = Data,
  case authorization_config:is_authorized(Username, Password) of
    true ->
      gen_tcp:send(State#state.socket, <<1:8, 0:8>>),
      {next_state, wait_socks_request, State#state{
        username = Username,
        password = Password
      }};
    false ->
      gen_tcp:send(State#state.socket, <<1:8, 16#ff:8>>),
      gen_tcp:shutdown(State#state.socket, read_write),
      stop
  end;
wait_authentication(_EventType, {tcp_closed, Socket}, _State) ->
  gen_tcp:shutdown(Socket, read_write),
  stop.

wait_socks_request(_EventType, {tcp, _Socket, Data}, State) ->
  ATyp = binary:at(Data, 3),
  case ATyp of
    1 ->
      <<_Ver:8, _Cmd:8, 0:8, _ATyp:8, DstAddrBin:4/binary, Port:16>> = Data,
      DstAddr = list_to_tuple([ X || <<X>> <= DstAddrBin ]);
    3 ->
      <<_Ver:8, _Cmd:8, 0:8, _ATyp:8, DLen:8, Dom:DLen/binary, Port:16>> = Data,
      DstAddr =
        case inet:gethostbyname(binary_to_list(Dom)) of
          {ok, Hostent} ->
            hd(Hostent#hostent.h_addr_list)
        end,
      DstAddrBin = inet4_octets(DstAddr);
    4 ->
      <<_Ver:8, _Cmd:8, 0:8, _ATyp:8, DstAddrBin:16/binary, Port:16>> = Data,
      DstAddr = list_to_tuple([ X || <<X>> <= DstAddrBin ])
  end,
  %% TODO: other results
  ConnectResult = gen_tcp:connect(DstAddr, Port,
    [ binary, {active, true},
      {ifaddr , State#state.if_addr},
      {packet, raw} ],
    2000),
  case ConnectResult of
    {ok, ServerSocket} ->
      {ok, {ServerAddr, ServerPort}} = inet:sockname(ServerSocket),
      gen_tcp:send(State#state.socket, <<5:8, 0:8, 0:8, 1:8, (inet4_octets(ServerAddr)):32, ServerPort:16>>),
      {next_state, data_exchange, State#state{ server_socket = ServerSocket }};
    {error,eaddrnotavail} ->
      io:format("EADDRNOTAVAIL ~p~n", [DstAddr]),
      gen_tcp:send(State#state.socket, <<5:8, 0:8, 0:8, 1:8, DstAddrBin:32, Port:16>>),
      stop
  end;
wait_socks_request(_EventType, {tcp_closed, Socket}, _State) ->
  io:format("SOCKET ~p CLOSED", [Socket]),
  stop.

data_exchange(_EventType, {tcp, Socket, Data}, State) when Socket =:= State#state.server_socket ->
  gen_tcp:send(State#state.socket, Data),
  {keep_state, State};
data_exchange(_EventType, {tcp, Socket, Data}, State) when Socket =:= State#state.socket ->
  gen_tcp:send(State#state.server_socket, Data),
  {keep_state, State};
%% TODO: do this in terminate
data_exchange(_EveentType, {tcp_closed, Socket}, State) when Socket =:= State#state.server_socket ->
  gen_tcp:shutdown(State#state.socket, read_write),
  stop;
data_exchange(_EveentType, {tcp_closed, Socket}, State) when Socket =:= State#state.socket ->
  gen_tcp:shutdown(State#state.server_socket, read_write),
  stop.

handle_event(_EventType, _EventContent, _StateName, State) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

callback_mode() ->
  state_functions.

%%%===================================================================
%%% Internal functions
%%%===================================================================

inet4_octets({Oct1, Oct2, Oct3, Oct4}) ->
  B1 = Oct1 bsl 24,
  B2 = Oct2 bsl 16,
  B3 = Oct3 bsl 8,
  B4 = Oct4,
  B1 + B2 + B3 + B4.
