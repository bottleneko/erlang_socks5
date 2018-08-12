%%%-------------------------------------------------------------------
%% @doc erlang_socks5 top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(socks5_sup).
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
      id => authorization_config,
      start => {authorization_config, start_link, []},
      restart => transient,
      shutdown => 2000,
      type => worker,
      module => [authorization_config]
    },
    #{
      id => socks5_listener_sup,
      start => {socks5_listener_sup, start_link, []},
      restart => transient,
      shutdown => 2000,
      type => worker,
      module => [socks5_config, socks5_listener, socks5_listener_sup]
    },
    #{
      id => socks5_connections_sup,
      start => {socks5_connection_sup, start_link, []},
      restart => transient,
      shutdown => 2000,
      type => supervisor,
      module => [socks5_config, socks5_statem, socks5_connections_sup, authorization_config]
    }
  ],
  {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
