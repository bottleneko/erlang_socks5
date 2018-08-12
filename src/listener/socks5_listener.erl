-module(socks5_listener).
-behaviour(gen_server).

-record(socks5_listener_state, {
  listen_socket     :: port(),
  route_destination :: socks5_config:route_destination(),
  connections = []  :: [port()]
}).

-type state() :: #socks5_listener_state{}.

%% API
-export([start_link/1]).

-export([
  init/1,
  handle_cast/2,
  terminate/2,
  handle_call/3,
  handle_info/2
]).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link(Route :: socks5_config:route()) -> {ok, Pid :: pid()}.
start_link(Route) ->
  gen_server:start_link(?MODULE, [Route], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([socks5_config:route()]) -> {ok, state()}.
init([{{Interface, Protocol, Port}, RouteDestinations}]) ->
  Address = get_addr(Interface, Protocol),

  {ok, ListenSocket} = gen_tcp:listen(Port, [
    binary,
    {ifaddr, Address},
    {packet, raw},
    {active, false},
    {reuseaddr, true},
    socks5_utils:to_inet(Protocol)
  ]),
  gen_server:cast(self(), accept_connection),
  {ok, #socks5_listener_state{listen_socket = ListenSocket, route_destination = RouteDestinations}}.

handle_cast(accept_connection, #socks5_listener_state{
  listen_socket = ListenSocket,
  route_destination = RouteDestinations} = State
) ->
  Accepted = gen_tcp:accept(ListenSocket, 2000),
  case Accepted of
    {ok, Socket} ->
      {ok, Pid} = supervisor:start_child(socks5_connection_sup, [Socket, RouteDestinations]),
      gen_tcp:controlling_process(Socket, Pid),
      gen_statem:cast(Pid, socket_delegated),
      gen_server:cast(self(), accept_connection),
      {noreply, State};
    {error, closed} ->
      {stop, socket_closed, State};
    {error, timeout} ->
      gen_server:cast(self(), accept_connection),
      {noreply, State}
  end.

terminate(_Reason, State) ->
  gen_tcp:close(State#socks5_listener_state.listen_socket).

%%====================================================================
%% gen_server callbacks (unused)
%%====================================================================

handle_call(Msg, From, State) ->
  error([Msg, From, State]).

handle_info(Msg, State) ->
  error([Msg, State]).

get_addr(InterfaceName, Protocol) ->
  {ok, Interfaces} = inet:getifaddrs(),
  Interface = proplists:get_value(InterfaceName, Interfaces),
  {addr, Addr} =
    case proplists:lookup_all(addr, Interface) of
      [] ->
        throw(["No addrs", InterfaceName]);
      Addrs ->
        Ipv4Addrs = lists:filter(
          fun({addr, Addr}) ->
            size(Addr) == socks5_utils:protocol_address_length(Protocol)
          end, Addrs),
        hd(Ipv4Addrs)
    end,
  Addr.