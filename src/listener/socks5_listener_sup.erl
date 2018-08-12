-module(socks5_listener_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  SupFlags = #{
    strategy => simple_one_for_one
  },
  ChildSpecs = [#{
    id => socks5_listener,
    start => {socks5_listener, start_link, []},
    restart => temporary,
    shutdown => 2000,
    type => worker,
    modiles => [socks5_listener]
  }],
  {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
