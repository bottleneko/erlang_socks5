-module(es5_connection).

-behaviour(gen_statem).
-behaviour(ranch_protocol).

-include_lib("kernel/include/inet.hrl").

-include("socks5_method_codes.hrl").

%%% API
-export([start_link/3]).

%%% gen_statem callbacks
-export([init/1,
         format_status/2,
         handle_event/4,
         terminate/3,
         code_change/4,
         callback_mode/0
        ]).

%%% States
-export([wait_auth_methods/3,
         wait_authentication/3,
         wait_socks_request/3,
         connect_data_exchange/3,
         udp_associate_data_exchange/3
        ]).

-define(SERVER, ?MODULE).

-record(es5_connection_data, {
  %% required
  transport             :: module(),
  socket                :: port(),
  socket_port           :: inet:port_number(),
  in_interface_address  :: inet:ip4_address(),
  out_interface_address :: inet:ip4_address(),
  username              :: binary()           | undefined,
  password              :: binary()           | undefined,
  % connect
  host_tcp_socket       :: port()             | undefined,
  % udp associate
  client_tcp_socket     :: port()             | undefined,
  client_udp_socket     :: port()             | undefined,
  host_udp_socket       :: port()             | undefined,
  client_ip             :: inet:ip4_address() | undefined,
  client_port           :: inet:port_number() | undefined,
  host_ip               :: inet:ip4_address() | undefined,
  host_port             :: inet:port_number() | undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(Ref, Transport, Opts) -> {ok, pid()} when
    Ref       :: ranch:ref(),
    Transport :: module(),
    Opts      :: [inet:ip4_address()].
start_link(Ref, Transport, Opts) ->
  {ok, proc_lib:spawn_link(?MODULE, init, [{Ref, Transport, Opts}])}.

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

-spec init({Ref, Transport, Opts}) -> no_return() when
    Ref       :: ranch:ref(),
    Transport :: module(),
    Opts      :: [inet:ip4_address()].
init({Ref, Transport, [InAddress, OutAddr] = _Opts}) ->
  process_flag(trap_exit, true),

  {ok, Socket} = ranch:handshake(Ref),
  Transport:setopts(Socket, [{active, true}]),

  {ok, {InAddress, BoundPort}} = inet:sockname(Socket),

  Data =
    #es5_connection_data{
       transport             = Transport,
       socket                = Socket,
       socket_port           = BoundPort,
       in_interface_address  = InAddress,
       out_interface_address = OutAddr
      },
  gen_statem:enter_loop(?MODULE, [], wait_auth_methods, Data, []).

-spec format_status(Opt, Params) -> State when
    Opt :: any(),
    Params :: [any()],
    State :: wait_auth_methods
           | wait_authentication
           | wait_socks_request
           | connect_data_exchange
           | udp_associate_data_exchange.
format_status(_Opt, [_PDict, StateName, _Data]) ->
  Status = StateName,
  Status.

-spec handle_event(EventType, EventContent, StateName, Data) -> stop when
    EventType    :: any(),
    EventContent :: any(),
    StateName    :: any(),
    Data         :: any().
handle_event(_EventType, _EventContent, _StateName, _Data) ->
  stop.

-spec terminate(Reason, State, Data) -> ok when
    Reason :: any(),
    State  :: wait_auth_methods
            | wait_authentication
            | wait_socks_request
            | connect_data_exchange
            | udp_associate_data_exchange,
    Data   :: #es5_connection_data{}.
terminate(_Reason, _State, Data) ->
  #es5_connection_data{
     host_tcp_socket = HostTcpSocket
    } = Data,

  gen_tcp:shutdown(HostTcpSocket, read_write),
  gen_tcp:close(HostTcpSocket),
  ok.

-spec code_change(OldVsn, State, Data, Extra) -> {ok, State, Data} when
    OldVsn :: any(),
    State  :: wait_auth_methods
            | wait_authentication
            | wait_socks_request
            | connect_data_exchange
            | udp_associate_data_exchange,
    Data   :: #es5_connection_data{},
    Extra  :: any().
code_change(_OldVsn, StateName, Data, _Extra) ->
  {ok, StateName, Data}.

-spec callback_mode() -> state_functions.
callback_mode() ->
  state_functions.

%%%===================================================================
%%% States
%%%===================================================================

%% @see https://www.ietf.org/rfc/rfc1928.txt
-spec wait_auth_methods(EventType, TcpMessage, Data) -> Result when
    EventType  :: any(),
    TcpMessage :: {tcp, port(), binary()},
    Data       :: #es5_connection_data{},
    Result     :: {next_state, State, Data} | stop,
    State      :: wait_authentication.
wait_auth_methods(_EventType, {tcp, _Socket, _TcpData}, Data) ->
  #es5_connection_data{
     transport = Transport,
     socket    = Socket
    } = Data,

  Transport:send(Socket, es5_protocol:request_auth()),

  {next_state, wait_authentication, Data};
wait_auth_methods(_EventType, {tcp_closed, _Socket}, _Data) ->
  stop.

%% @see https://www.ietf.org/rfc/rfc1929.txt
-spec wait_authentication(EventType, TcpMessage, Data) -> Result when
    EventType  :: any(),
    TcpMessage :: {tcp, port(), binary()},
    Data       :: #es5_connection_data{},
    Result     :: {next_state, State, Data} | stop,
    State      :: wait_socks_request.
wait_authentication(_EventType, {tcp, _Socket, TcpData}, Data) ->
  #es5_connection_data{
     transport = Transport,
     socket    = Socket
    } = Data,

  {Username, Password} = es5_protocol:get_credentials(TcpData),
  case es5_authentication_server:is_authenticated(Username, Password) of
    true ->
      Transport:send(Socket, es5_protocol:success_authentication()),

      NewData = Data#es5_connection_data{
        username = Username,
        password = Password
      },
      {next_state, wait_socks_request, NewData};
    false ->
      Transport:send(Socket, es5_protocol:authentication_fail()),

      stop
  end;
wait_authentication(_EventType, {tcp_closed, _Socket}, _Data) ->
  stop.

-spec wait_socks_request(EventType, TcpMessage, Data) -> Result when
    EventType  :: any(),
    TcpMessage :: {tcp, port(), binary()},
    Data       :: #es5_connection_data{},
    Result     :: {next_state, State, Data} | stop,
    State      :: connect_data_exchange | udp_associate_data_exchange.
wait_socks_request(_EventType, {tcp, _Socket, TcpData}, Data) ->
  {DstAddr, Port} = es5_protocol:get_destination_endpoint(TcpData),
  Cmd = es5_protocol:get_cmd(TcpData),
  request_command_processing(Cmd, DstAddr, Port, Data);
wait_socks_request(_EventType, {tcp_closed, _Socket}, _Data) ->
  stop.

-spec connect_data_exchange(EventType, TcpMessage, Data) -> Result when
    EventType  :: any(),
    TcpMessage :: {tcp, port(), binary()},
    Data       :: #es5_connection_data{},
    Result     :: {keep_state, Data} | stop.
connect_data_exchange(_EventType, {tcp, _Socket, _TcpData} = TcpMessage, Data) ->
  #es5_connection_data{
     transport       = Transport,
     socket          = Socket,
     host_tcp_socket = HostSocket
    } = Data,

  forward(Transport, Socket, HostSocket, TcpMessage),
  {keep_state, Data};
connect_data_exchange(_EventType, {tcp_closed, Socket}, Data) when
    Socket =:= Data#es5_connection_data.host_tcp_socket;
    Socket =:= Data#es5_connection_data.socket ->
  stop.

-spec udp_associate_data_exchange(EventType, TcpMessage, Data) -> Result when
    EventType  :: any(),
    TcpMessage :: {tcp, port(), binary()},
    Data       :: #es5_connection_data{},
    Result     :: {keep_state, Data} | stop.
udp_associate_data_exchange(_EventType, {udp, Socket, Address, Port, UdpData}, Data) when
    Socket  =:= Data#es5_connection_data.client_udp_socket,
    Address =:= Data#es5_connection_data.host_ip,
    Port    =:= Data#es5_connection_data.host_port ->
  gen_udp:send(Data#es5_connection_data.client_udp_socket, Address, Port, UdpData),
  {keep_state, Data};
udp_associate_data_exchange(_EventType, {udp, _Socket, Address, Port, _UdpData}, Data) ->
  error_logger:warning_msg("Illegitimate dgram from: ~p on: ~p~n", [Address, Port]),
  {keep_state, Data};
udp_associate_data_exchange(_EventType, {tcp_closed, Socket}, Data) when
    Socket =:= Data#es5_connection_data.client_tcp_socket ->
  stop.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec forward(Transport, Socket, HostSocket, TcpMessage) -> ok when
    Transport  :: module(),
    Socket     :: port(),
    HostSocket :: port(),
    TcpMessage :: {tcp, port(), Data},
    Data       :: binary().
forward(Transport, Socket, HostSocket, {tcp, Socket, TcpData}) ->
  Transport:send(HostSocket, TcpData);
forward(Transport, Socket, HostSocket, {tcp, HostSocket, TcpData}) ->
  Transport:send(Socket, TcpData).

-spec request_command_processing(Cmd, HostAddress, HostPort, Data) -> Result when
    Cmd         :: es5_protocol:cmd(),
    HostAddress :: inet:ip4_address(),
    HostPort    :: inet:port_number(),
    Data        :: #es5_connection_data{},
    Result      :: {next_state, State, Data} | stop,
    State       :: connect_data_exchange.
request_command_processing(connect, HostAddress, HostPort, Data) ->
  #es5_connection_data{
     transport              = Transport,
     socket                 = Socket,
     socket_port            = BoundPort,
     in_interface_address   = InAddress,
     out_interface_address  = OutAddress
    } = Data,

  SocketOpts =
    [binary,
     {active, true},
     {ifaddr, OutAddress},
     {packet, raw}
    ],
  ConnectResult = gen_tcp:connect(HostAddress, HostPort, SocketOpts, 2000),
  case ConnectResult of
    {ok, ServerSocket} ->
      TcpResponse = es5_protocol:success_connect(InAddress, BoundPort),
      Transport:send(Socket, TcpResponse),

      {next_state, connect_data_exchange, Data#es5_connection_data{host_tcp_socket = ServerSocket}};
    {error,eaddrnotavail} ->
      error_logger:info_msg("eaddrnotavail: ~p~n", [HostAddress]),

      TcpResponse = es5_protocol:host_not_available(InAddress, BoundPort),
      Transport:send(Socket, TcpResponse),

      stop
  end;
%% TODO: make support
request_command_processing(bind, _HostAddress, _HostPort, Data) ->
  #es5_connection_data{
     transport            = Transport,
     socket               = Socket,
     socket_port          = BoundPort,
     in_interface_address = InAddress
    } = Data,

  TcpResponse = es5_protocol:command_not_available(InAddress, BoundPort),
  Transport:send(Socket, TcpResponse),

  stop;
%% TODO: make support
request_command_processing(udp_associate, HostAddress, HostPort, Data) ->
  #es5_connection_data{
     transport             = Transport,
     socket                = Socket,
     in_interface_address  = InAddress,
     out_interface_address = OutAddress
    } = Data,

  ClientUdpSocket = make_udp_socket(InAddress),
  HostUdpSocket = make_udp_socket(OutAddress),
  {ok, {ClientAddress, _}} = inet:sockname(Socket),
  {ok, {_BoundAddress, BoundPort}} = inet:sockname(ClientUdpSocket),

  TcpResponse = es5_protocol:success_connect(InAddress, BoundPort),
  Transport:send(Socket, TcpResponse),

  NextData =
    Data#es5_connection_data{
      client_udp_socket = ClientUdpSocket,
      host_udp_socket   = HostUdpSocket,
      client_ip         = ClientAddress,
      client_port       = BoundPort,
      host_ip           = HostAddress,
      host_port         = HostPort
     },
  {next_state, udp_associate_data_exchange, NextData};
request_command_processing(unsupported, _HostAddress, _HostPort, Data) ->
  #es5_connection_data{
     transport            = Transport,
     socket               = Socket,
     socket_port          = BoundPort,
     in_interface_address = InAddress
    } = Data,

  TcpResponse = es5_protocol:command_not_available(InAddress, BoundPort),
  Transport:send(Socket, TcpResponse),

  stop.

make_udp_socket(Interface) ->
  SocketOpts =
    [binary,
     {active, true},
     {ifaddr, Interface}
    ],
  {ok, UdpSocket} = gen_udp:open(0, SocketOpts),
  UdpSocket.
