%%%-------------------------------------------------------------------
%% @doc erlang_socks5 top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlang_socks5_sup).

-behaviour(supervisor).

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
      id => socks5,
      start => {socks5, start_link, [{127,0,0,1}, 10800]},
      restart => transient,
      shutdown => 2000,
      type => worker,
      module => [socks5, socks5_statem, socks5_connections_sup]
    },
    #{
      id => socks5_connections_sup,
      start => {socks5_connections_sup, start_link, []},
      restart => transient,
      shutdown => 2000,
      type => supervisor,
      module => [socks5_statem, socks5_connections_sup]
    }
  ],
  {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================