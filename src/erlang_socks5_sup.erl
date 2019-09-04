%%%-------------------------------------------------------------------
%% @doc erlang_socks5 top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlang_socks5_sup).
-behavior(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  SupFlags = #{
    strategy => one_for_one
  },
  ChildSpecs = [
    #{
      id => es5_authorization_config,
      start => {es5_authorization_config, start_link, []},
      restart => transient,
      shutdown => 2000,
      type => worker,
      module => [es5_authorization_config]
    }
  ],
  {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
