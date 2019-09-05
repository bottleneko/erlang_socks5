-module(es5_authentication_server).
-behaviour(gen_server).

%% API
-export([
  start_link/0,
  is_authenticated/2
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

-record(es5_authentication_server_state, {
  accounts_tid
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec is_authenticated(Username, Password) -> boolean() when
    Username :: binary(),
    Password :: binary().
is_authenticated(Username, Password) ->
  gen_server:call(?MODULE, {is_authenticated, Username, Password}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([]) -> {ok, #es5_authentication_server_state{}}.
init([]) ->
  Accounts = es5_config:accounts(),
  Tid = ets:new(accounts, []),
  Foreach =
    fun(Elem) ->
        ets:insert(Tid, Elem)
    end,
  lists:foreach(Foreach, Accounts),
  State = #es5_authentication_server_state{accounts_tid = Tid},
  {ok, State}.

-spec handle_call(Message, From, State) -> Result when
    Message  :: {is_authorized, Username, Password},
    Username :: binary(),
    Password :: binary(),
    From     :: any(),
    Result   :: {reply, boolean(), State},
    State    :: #es5_authentication_server_state{}.
handle_call({is_authenticated, Username, Password}, _From, State) ->
  Reply =
    case ets:lookup(State#es5_authentication_server_state.accounts_tid, Username) of
      [{Username, Password}] ->
        true;
      _ ->
        false
    end,
  {reply, Reply, State}.

-spec handle_cast(Message, State) -> {noreply, State} when
    Message :: any(),
    State   :: any().
handle_cast(_Message, State) ->
  {noreply, State}.

-spec handle_info(Message, State) -> {noreply, State} when
    Message :: any(),
    State   :: any().
handle_info(_Message, State) ->
  {noreply, State}.

-spec terminate(Reason, State) -> ok when
    Reason :: any(),
    State  :: any().
terminate(_Reason, _State) ->
  ok.

-spec code_change(OldVsn, State, Extra) -> {ok, State} when
    OldVsn :: any(),
    State  :: any(),
    Extra  :: any(),
    State  :: any().
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
