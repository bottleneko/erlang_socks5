-module(socks5_statem).
-behaviour(gen_statem).

-include_lib("kernel/include/inet.hrl").

-include("socks5_method_codes.hrl").

%% API
-export([start_link/3]).

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
  connect_data_exchange/3,
  udp_associate_data_exchange/3
]).

-define(SERVER, ?MODULE).

-record(state, {
  %% required
  socket,
  in_interface_address,
  out_interface_address,
  username,
  password,
  %% depends by client cmd
  cmd,
  % connect
  host_tcp_socket,
  % udp associate
  client_tcp_socket,
  client_udp_socket,
  host_udp_socket,
  client_ip,
  client_port,
  host_ip,
  host_port
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(InAddr, OutAddr, Socket) ->
  gen_statem:start_link(?MODULE, [InAddr, OutAddr, Socket], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

init([InAddr, OutAddr, Socket]) ->
  process_flag(trap_exit, true),
  {ok, wait_for_socket_control,
    #state{
      socket = Socket,
      in_interface_address = InAddr,
      out_interface_address = OutAddr
  }}.

format_status(_Opt, [_PDict, StateName, _State]) ->
  Status = StateName,
  Status.

wait_for_socket_control(_EventType, socket_delegated, State) ->
  inet:setopts(State#state.socket, [{active, true}]),
  {next_state, wait_auth_methods, State}.

%% rfc 1928
wait_auth_methods(_EventType, {tcp, _Socket, _Data}, State) ->
  gen_tcp:send(State#state.socket, [<<?PROTOCOL_VERSION>>, <<?USERNAME_PASSWORD_AUTH>>]),
  {next_state, wait_authentication, State};
wait_auth_methods(_EventType, {tcp_closed, _Socket}, _State) ->
  stop.

%% rfc 1929
wait_authentication(_EventType, {tcp, _Socket, Data}, State) ->
  <<1, ULen, Username:ULen/binary, PLen, Password:PLen/binary>> = Data,
  case authorization_config:is_authorized(Username, Password) of
    true ->
      gen_tcp:send(State#state.socket, <<?USERNAME_PASSWORD_AUTH_VERSION, ?USERNAME_PASSWORD_AUTH_SUCCESS>>),
      {next_state, wait_socks_request, State#state{
        username = Username,
        password = Password
      }};
    false ->
      gen_tcp:send(State#state.socket, <<?USERNAME_PASSWORD_AUTH_VERSION, ?USERNAME_PASSWORD_AUTH_FAIL>>),
      stop
  end;
wait_authentication(_EventType, {tcp_closed, _Socket}, _State) ->
  stop.

wait_socks_request(_EventType, {tcp, _Socket, Data}, State) ->
  {DstAddr, Port} = get_destination_endpoint(Data),
  Cmd = get_cmd(Data),
  request_command_processing(Cmd, DstAddr, Port, State);
wait_socks_request(_EventType, {tcp_closed, _Socket}, _State) ->
  stop.

connect_data_exchange(_EventType, {tcp, Socket, Data}, State) when Socket =:= State#state.host_tcp_socket ->
  gen_tcp:send(State#state.socket, Data),
  {keep_state, State};
connect_data_exchange(_EventType, {tcp, Socket, Data}, State) when Socket =:= State#state.socket ->
  gen_tcp:send(State#state.host_tcp_socket, Data),
  {keep_state, State};
connect_data_exchange(_EventType, {tcp_closed, Socket}, State) when Socket =:= State#state.host_tcp_socket ->
  stop;
connect_data_exchange(_EventType, {tcp_closed, Socket}, State) when Socket =:= State#state.socket ->
  stop.

udp_associate_data_exchange(_EventType, {tcp_closed, Socket}, State) when Socket =:= State#state.client_tcp_socket ->
  stop;
udp_associate_data_exchange(_EventType, {udp, Socket, Address, Port, Data}, State) when
  Socket =:= State#state.client_udp_socket,
  Address =:= State#state.client_ip,
  Port =:= State#state.client_port ->
  gen_udp:send(State#state.client_udp_socket, State#state.host_ip, State#state.host_port, Data),
  {keep_state, State};
udp_associate_data_exchange(_EventType, {udp, _Socket, Address, Port, _Data}, State) ->
  error_logger:warning_msg("Illegitimate dgram from: ~p on: ~p~n", [Address, Port]),
  {keep_state, State}.

handle_event(_EventType, _EventContent, _StateName, State) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

terminate(_Reason, _StateName, State) ->
  gen_tcp:shutdown(State#state.host_tcp_socket, read_write),
  gen_tcp:close(State#state.host_tcp_socket),
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

callback_mode() ->
  state_functions.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_destination_endpoint(Request) ->
  ATyp = binary:at(Request, 3),
  case ATyp of
    ?QUERY_IPv4 ->
      <<?PROTOCOL_VERSION, _Cmd, ?RESERVED, ATyp, DstAddrBin:4/binary, Port:16>> = Request,
      DstAddr = list_to_tuple([ X || <<X>> <= DstAddrBin ]);
    ?QUERY_FQDN ->
      <<?PROTOCOL_VERSION, _Cmd, ?RESERVED, ATyp, DLen, Dom:DLen/binary, Port:16>> = Request,
      DstAddr =
        case inet:gethostbyname(binary_to_list(Dom)) of
          {ok, Hostent} ->
            hd(Hostent#hostent.h_addr_list)
        end;
    ?QUERY_IPv6 ->
      <<?PROTOCOL_VERSION, _Cmd, ?RESERVED, ATyp, DstAddrBin:16/binary, Port:16>> = Request,
      DstAddr = list_to_tuple([ X || <<X>> <= DstAddrBin ])
  end,
  {DstAddr, Port}.

get_cmd(Request) ->
  case binary:at(Request, 1) of
    ?CONNECT_CMD ->
      connect;
    ?BIND_CMD ->
      bind;
    ?UDP_ASSOC_CMD ->
      udp_associate;
    _ ->
      unsupported
  end.

request_command_processing(connect, HostAddress, HostPort, State) ->
  ConnectResult = gen_tcp:connect(HostAddress, HostPort,
    [ binary, {active, true},
      {ifaddr , State#state.in_interface_address},
      {packet, raw} ],
    2000),
  case ConnectResult of
    {ok, ServerSocket} ->
      BoundAddress = inet4_octets(State#state.in_interface_address),
      {ok, {_, BoundPort}} = inet:sockname(State#state.socket),
      gen_tcp:send(State#state.socket,
        <<?PROTOCOL_VERSION, ?SUCCESS_CONNECT, ?RESERVED, ?QUERY_IPv4, BoundAddress:32, BoundPort:16>>),
      {next_state, connect_data_exchange, State#state{ host_tcp_socket = ServerSocket }};
    {error,eaddrnotavail} ->
      BoundAddress = inet4_octets(State#state.in_interface_address),
      {ok, {_, BoundPort}} = inet:sockname(State#state.socket),
      error_logger:info_msg("eaddrnotavail: ~p~n", [HostAddress]),
      gen_tcp:send(State#state.socket,
        <<?PROTOCOL_VERSION, ?HOST_NOT_AVAIL, ?RESERVED, ?QUERY_IPv4, (BoundAddress):32, BoundPort:16>>),
      stop
  end;
%% TODO: make support
request_command_processing(bind, _HostAddress, _HostPort, State) ->
  BoundAddress = inet4_octets(State#state.in_interface_address),
  {ok, {BoundAddress, BoundPort}} = inet:sockname(State#state.socket),
  gen_tcp:send(State#state.socket,
    <<?PROTOCOL_VERSION, ?COMMAND_NOT_AVAIL, ?RESERVED, ?QUERY_IPv4, BoundAddress:32, BoundPort:16>>),
  stop;
%% TODO: make support
request_command_processing(udp_associate, HostAddress, HostPort, State) ->
  {udp_client, ClientUdpSocket} = gen_udp:open(0, [binary, {active, true}, {ifaddr, State#state.in_interface_address}]),
  {udp_host, HostUdpSocket} = gen_udp:open(0, [binary, {active, true}, {ifaddr, State#state.out_interface_address}]),
  BoundAddress = inet4_octets(State#state.in_interface_address),
  {ok, {ClientAddress, _}} = inet:sockname(State#state.socket),
  {ok, {BoundAddress, BoundPort}} = inet:sockname(ClientUdpSocket),
  gen_tcp:send(State#state.socket,
    <<?PROTOCOL_VERSION, ?SUCCESS_CONNECT, ?RESERVED, ?QUERY_IPv4, BoundAddress, BoundPort:16>>),
  {next_state, udp_associate_data_exchange,
    State#state{
      client_udp_socket = ClientUdpSocket,
      host_udp_socket = HostUdpSocket,
      client_ip = ClientAddress,
      client_port = BoundPort,
      host_ip = HostAddress,
      host_port = HostPort
  }};
request_command_processing(unsupported, _HostAddress, _HostPort, State) ->
  BoundAddress = inet4_octets(State#state.in_interface_address),
  {ok, {BoundAddress, BoundPort}} = inet:sockname(State#state.socket),
  gen_tcp:send(State#state.socket,
    <<?PROTOCOL_VERSION, ?COMMAND_NOT_AVAIL, ?RESERVED, ?QUERY_IPv4, BoundAddress:32, BoundPort:16>>),
  stop.

inet4_octets({Oct1, Oct2, Oct3, Oct4}) ->
  B1 = Oct1 bsl 24,
  B2 = Oct2 bsl 16,
  B3 = Oct3 bsl 8,
  B4 = Oct4,
  B1 + B2 + B3 + B4.
