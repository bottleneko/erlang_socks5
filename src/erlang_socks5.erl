-module(erlang_socks5).
-behaviour(gen_server).

-record(state, {
  listen_socket,
  connections = []
}).

%% API
-export([
  start_link/2
]).

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

start_link(Address, Port) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Address, Port], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Address, Port]) ->
  {ok, ListenSocket} = gen_tcp:listen(Port, [
    binary,
    {ifaddr, Address},
    {packet, raw},
    {active, false},
    {reuseaddr, true}
  ]),
  gen_server:cast(?MODULE, accept_connection),
  {ok, #state{listen_socket = ListenSocket}}.

handle_cast(accept_connection, State) ->
  Accepted = gen_tcp:accept(State#state.listen_socket, 2000),
  case Accepted of
    {ok, Socket} ->
      {ok, Pid} = supervisor:start_child(es5_connections_sup, [Socket]),
      gen_tcp:controlling_process(Socket, Pid),
      gen_statem:cast(Pid, socket_delegated),
      gen_server:cast(?MODULE, accept_connection),
      {noreply, State};
    {error, closed} ->
      {stop, socket_closed, State};
    {error, timeout} ->
      gen_server:cast(?MODULE, accept_connection),
      {noreply, State}
  end.

terminate(_Reason, State) ->
  gen_tcp:close(State#state.listen_socket).

%%====================================================================
%% gen_server callbacks (unused)
%%====================================================================

handle_call(Msg, From, State) ->
  error([Msg, From, State]).

handle_info(Msg, State) ->
  error([Msg, State]).
