%%%-------------------------------------------------------------------
%% @doc erlang_socks5 top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlang_socks5_sup).
-behavior(supervisor).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Port, InAddr, OutAddr) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [{port, Port}, {in_addr, InAddr}, {out_addr, OutAddr}]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([{port, Port}, {in_addr, InAddr}, {out_addr, OutAddr}]) ->
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
    },
    #{
      id => erlang_socks5,
      start => {erlang_socks5, start_link, [InAddr, Port]},
      restart => transient,
      shutdown => 2000,
      type => worker,
      module => [erlang_socks5, es5_statem, es5_connections_sup, es5_authorization_config]
    },
    #{
      id => es5_connections_sup,
      start => {es5_connections_sup, start_link, [InAddr, OutAddr]},
      restart => transient,
      shutdown => 2000,
      type => supervisor,
      module => [es5_statem, es5_connections_sup, es5_authorization_config]
    }
  ],
  {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
