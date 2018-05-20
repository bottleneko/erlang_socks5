-module(socks5_statem).
-behaviour(gen_statem).

%% API
-export([start_link/1]).

%% gen_statem callbacks
-export([
  init/1,
  format_status/2,
  state_name/3,
  handle_event/4,
  terminate/3,
  code_change/4,
  callback_mode/0
]).

-define(SERVER, ?MODULE).

-record(state, {
  socket
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Socket) ->
  gen_statem:start_link(?MODULE, [Socket], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

init([Socket]) ->
  {ok, state_name, #state{ socket = Socket}}.

format_status(_Opt, [_PDict, _StateName, _State]) ->
  Status = some_term,
  Status.

state_name(_EventType, _EventContent, State) ->
  NextStateName = next_state,
  {next_state, NextStateName, State}.

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
