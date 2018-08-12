-module(authorization_config).
-behaviour(gen_server).

%% API
-export([
  start_link/0,
  is_authorized/2
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {
  accounts_tid
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

is_authorized(Username, Password) ->
  gen_server:call(?MODULE, {authorized, Username, Password}).

init([]) ->
  {ok, Config} = socks5_config:by_interface_name("lo0"),
  Params = socks5_config:get_config_params(Config),
  {ok, Accounts} = socks5_config:get_config_authentication_credentials(Params),

  Tid = ets:new(accounts, []),
  lists:foreach(
    fun(Elem) ->
      ets:insert(Tid, Elem)
    end, Accounts),
  {ok, #state{
    accounts_tid = Tid
  }}.

handle_call({authorized, Username, Password}, _From, State) ->
  Reply =
    case ets:lookup(State#state.accounts_tid, Username) of
      [{Username, Password}] ->
        true;
      _ ->
        false
    end,
  {reply, Reply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
