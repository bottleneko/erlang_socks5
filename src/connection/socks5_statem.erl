-module(socks5_statem).
-behaviour(gen_statem).

-include_lib("kernel/include/inet.hrl").

-include("socks5_method_codes.hrl").

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
  connect_data_exchange/3,
  udp_associate_data_exchange/3
]).

-define(SERVER, ?MODULE).

-record(socks5_statem_state, {
  %% required
  socket :: port(),
  route :: socks5_config:route(),
  in_interface_address :: tuple(),
  out_interface_address :: tuple(),
  username :: string(),
  password :: string(),
  %% depends by client cmd
  cmd :: non_neg_integer(),
  % connect
  host_tcp_socket :: port(),
  % udp associate
  client_udp_socket :: port(),
  client_udp_ip :: tuple(),
  client_udp_port :: non_neg_integer(),
  host_udp_socket :: port(),
  host_udp_ip :: tuple(),
  host_udp_port :: non_neg_integer()
}).

-type data() :: #socks5_statem_state{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(Socket :: port(), Route :: socks5_config:route()) ->
  {ok, Pid :: pid()}.
start_link(Socket, Route) ->
  gen_statem:start_link(?MODULE, [Socket, Route], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

-spec init([port() | socks5_config:route()]) ->
  {ok, State :: atom(), Data :: data()}.
init([Socket, Route]) ->
  process_flag(trap_exit, true),
  {ok, wait_for_socket_control, #socks5_statem_state{
    socket = Socket,
    route = Route
  }}.

format_status(_Opt, [_PDict, StateName, _State]) ->
  Status = StateName,
  Status.

wait_for_socket_control(_EventType, socket_delegated,
    #socks5_statem_state{
      socket = Socket
    } = State) ->
  inet:setopts(Socket, [{active, true}]),
  {ok, {InAddress, _InPort}} = inet:sockname(Socket),
  {next_state, wait_auth_methods,
    State#socks5_statem_state{in_interface_address = InAddress}}.

%% rfc 1928
wait_auth_methods(_EventType, {tcp, _Socket, _Data}, State) ->
  gen_tcp:send(State#socks5_statem_state.socket, [<<?PROTOCOL_VERSION>>, <<?USERNAME_PASSWORD_AUTH>>]),
  {next_state, wait_authentication, State};
wait_auth_methods(_EventType, {tcp_closed, _Socket}, _State) ->
  stop.

%% rfc 1929
wait_authentication(_EventType, {tcp, _Socket, Data}, State) ->
  <<1, ULen, Username:ULen/binary, PLen, Password:PLen/binary>> = Data,
  case authorization_config:is_authorized(Username, Password) of
    true ->
      gen_tcp:send(State#socks5_statem_state.socket, <<
        ?USERNAME_PASSWORD_AUTH_VERSION,
        ?USERNAME_PASSWORD_AUTH_SUCCESS
      >>),

      {next_state, wait_socks_request, State#socks5_statem_state{
        username = Username,
        password = Password
      }};
    false ->
      gen_tcp:send(State#socks5_statem_state.socket, <<
        ?USERNAME_PASSWORD_AUTH_VERSION,
        ?USERNAME_PASSWORD_AUTH_FAIL
      >>),

      stop
  end;
wait_authentication(_EventType, {tcp_closed, _Socket}, _State) ->
  stop.

wait_socks_request(_EventType, {tcp, _Socket, Data}, #socks5_statem_state{route = Route} = State) ->
  {DstAddr, Port} = get_destination_endpoint(Data),
  {ok, Interface} = determine_route_destination(DstAddr, Route),

  {ok, Protocol} = socks5_utils:address_protocol(DstAddr),
  {ok, OutAddress} = socks5_utils:get_interface_address(Protocol, Interface),

  {ok, Cmd} = get_cmd(Data),

  request_command_processing(Cmd, DstAddr, Port, State#socks5_statem_state{
    out_interface_address = OutAddress
  });
wait_socks_request(_EventType, {tcp_closed, _Socket}, _State) ->
  stop.

connect_data_exchange(_EventType, {tcp, Socket, Data}, State) when Socket =:= State#socks5_statem_state.host_tcp_socket ->
  gen_tcp:send(State#socks5_statem_state.socket, Data),

  {keep_state, State};
connect_data_exchange(_EventType, {tcp, Socket, Data}, State) when Socket =:= State#socks5_statem_state.socket ->
  gen_tcp:send(State#socks5_statem_state.host_tcp_socket, Data),

  {keep_state, State};
connect_data_exchange(_EventType, {tcp_closed, Socket}, State) when Socket =:= State#socks5_statem_state.host_tcp_socket ->
  stop;
connect_data_exchange(_EventType, {tcp_closed, Socket}, State) when Socket =:= State#socks5_statem_state.socket ->
  stop.

udp_associate_data_exchange(_EventType, {tcp_closed, TcpSocket},
    #socks5_statem_state{
      socket = Socket
    } = _State) when TcpSocket =:= Socket ->
  stop;
udp_associate_data_exchange(_EventType, {udp, Socket, Address, _Port, Data},
    #socks5_statem_state{
      client_udp_socket = ClientUdpSocket,
      client_udp_ip = ClientAddress,
      host_udp_socket = HostUdpSocket,
      host_udp_ip = HostUdpAddress,
      host_udp_port = HostUdpPort
    } = State) when
  Socket =:= ClientUdpSocket,
  Address =:= ClientAddress
  ->
  gen_udp:send(HostUdpSocket, HostUdpAddress, HostUdpPort, Data),

  {keep_state, State};
udp_associate_data_exchange(_EventType, {udp, _Socket, Address, Port, _Data}, State) ->
  error_logger:warning_msg("Illegitimate dgram from: ~p on: ~p~n", [Address, Port]),
  {keep_state, State}.

handle_event(_EventType, _EventContent, _StateName, State) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

terminate(_Reason, _StateName, State) ->
  catch gen_tcp:shutdown(State#socks5_statem_state.host_tcp_socket, read_write),
  catch gen_tcp:close(State#socks5_statem_state.host_tcp_socket),
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

callback_mode() ->
  state_functions.

%%%===================================================================
%%% Internal functions
%%%===================================================================

determine_route_destination(DstAddr, RouteDestinations) ->
  {ok, Protocol} = socks5_utils:address_protocol(DstAddr),
  case lists:keyfind(Protocol, 2, RouteDestinations) of
    false ->
      {error, not_allowed};
    {Interface, _Protocol} ->
      {ok, Interface}
  end.

get_destination_endpoint(Request) ->
  ATyp = binary:at(Request, 3),
  case ATyp of
    ?QUERY_IPv4 ->
      <<?PROTOCOL_VERSION, _Cmd, ?RESERVED, ATyp, DstAddrBin:4/binary, Port:16>> = Request,
      DstAddr = list_to_tuple([X || <<X>> <= DstAddrBin]);
    ?QUERY_FQDN ->
      <<?PROTOCOL_VERSION, _Cmd, ?RESERVED, ATyp, DLen, Dom:DLen/binary, Port:16>> = Request,
      DstAddr =
        case inet:gethostbyname(binary_to_list(Dom)) of
          {ok, Hostent} ->
            hd(Hostent#hostent.h_addr_list)
        end;
    ?QUERY_IPv6 ->
      <<?PROTOCOL_VERSION, _Cmd, ?RESERVED, ATyp, DstAddrBin:8/binary-unit:16, Port:16>> = Request,
      DstAddr = list_to_tuple([X || <<X:16>> <= DstAddrBin])
  end,
  {DstAddr, Port}.

-spec get_cmd(Request :: binary()) ->
  {ok, connect | bind | udp_accociate} | {error, not_supported}.
get_cmd(Request) ->
  case binary:at(Request, 1) of
    ?CONNECT_CMD ->
      {ok, connect};
    ?BIND_CMD ->
      {ok, bind};
    ?UDP_ASSOC_CMD ->
      {ok, udp_associate};
    _ ->
      {error, not_supported}
  end.

request_command_processing(connect, HostAddress, HostPort,
    #socks5_statem_state{
      socket = Socket,
      in_interface_address = InAddress,
      out_interface_address = OutAddress
    } = State) ->

  {ok, Protocol} = socks5_utils:address_protocol(HostAddress),

  ConnectResult = gen_tcp:connect(HostAddress, HostPort, [
    binary,
    {active, true},
    {ifaddr, OutAddress},
    {packet, raw},
    socks5_utils:to_inet(Protocol)
  ], 2000),

  case ConnectResult of
    {ok, ServerSocket} ->
      BoundAddress = socks5_utils:address_number(InAddress),
      {ok, {_, BoundPort}} = inet:sockname(Socket),

      gen_tcp:send(Socket, <<
        ?PROTOCOL_VERSION,
        ?SUCCESS_CONNECT,
        ?RESERVED,
        ?QUERY_IPv4,
        BoundAddress:32,
        BoundPort:16
      >>),

      {next_state, connect_data_exchange, State#socks5_statem_state{host_tcp_socket = ServerSocket}};
    {error, eaddrnotavail} ->
      BoundAddress = socks5_utils:address_number(InAddress),
      {ok, {_, BoundPort}} = inet:sockname(Socket),

      error_logger:info_msg("eaddrnotavail: ~p~n", [HostAddress]),

      gen_tcp:send(State#socks5_statem_state.socket, <<
        ?PROTOCOL_VERSION,
        ?HOST_NOT_AVAIL,
        ?RESERVED,
        ?QUERY_IPv4,
        BoundAddress:32,
        BoundPort:16
      >>),

      stop
  end;
request_command_processing(bind, _HostAddress, _HostPort, State) ->
  BoundAddress = socks5_utils:address_number(State#socks5_statem_state.in_interface_address),
  {ok, {BoundAddress, BoundPort}} = inet:sockname(State#socks5_statem_state.socket),

  gen_tcp:send(State#socks5_statem_state.socket, <<
    ?PROTOCOL_VERSION,
    ?COMMAND_NOT_AVAIL,
    ?RESERVED,
    ?QUERY_IPv4,
    BoundAddress:32,
    BoundPort:16
  >>),

  stop;
request_command_processing(udp_associate, HostAddress, HostPort,
    #socks5_statem_state{
      socket = Socket,
      in_interface_address = Address,
      out_interface_address = OutAddress
    } = State) ->
  {ok, ClientUdpSocket} = gen_udp:open(0, [binary, {active, true}, {ifaddr, Address}]),
  {ok, HostUdpSocket} = gen_udp:open(0, [binary, {active, true}, {ifaddr, OutAddress}]),
  {ok, {ClientAddress, _}} = inet:sockname(Socket),
  {ok, {Address, ClientPort}} = inet:sockname(ClientUdpSocket),

  {ok, Protocol} = socks5_utils:address_protocol(Address),

  case Protocol of
    ipv4 ->
      gen_tcp:send(State#socks5_statem_state.socket, <<
        ?PROTOCOL_VERSION,
        ?SUCCESS_CONNECT,
        ?RESERVED,
        ?QUERY_IPv4,
        (socks5_utils:address_number(Address)):32,
        ClientPort:16
      >>);
    ipv6 ->
      gen_tcp:send(State#socks5_statem_state.socket, <<
        ?PROTOCOL_VERSION,
        ?SUCCESS_CONNECT,
        ?RESERVED,
        ?QUERY_IPv6,
        (socks5_utils:address_number(Address)):128,
        ClientPort:16
      >>)
  end,

  {next_state, udp_associate_data_exchange,
    State#socks5_statem_state{
      client_udp_socket = ClientUdpSocket,
      client_udp_ip = ClientAddress,
      client_udp_port = ClientPort,
      host_udp_socket = HostUdpSocket,
      host_udp_ip = HostAddress,
      host_udp_port = HostPort
    }};
request_command_processing(unsupported, _HostAddress, _HostPort, State) ->
  BoundAddress = socks5_utils:address_number(State#socks5_statem_state.in_interface_address),
  {ok, {BoundAddress, BoundPort}} = inet:sockname(State#socks5_statem_state.socket),

  gen_tcp:send(State#socks5_statem_state.socket, <<
    ?PROTOCOL_VERSION,
    ?COMMAND_NOT_AVAIL,
    ?RESERVED,
    ?QUERY_IPv4,
    BoundAddress:32,
    BoundPort:16
  >>),

  stop.