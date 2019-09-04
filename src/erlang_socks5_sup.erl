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
      id => authorization_config,
      start => {authorization_config, start_link, []},
      restart => transient,
      shutdown => 2000,
      type => worker,
      module => [authorization_config]
    },
    #{
      id => erlang_socks5,
      start => {erlang_socks5, start_link, [InAddr, Port]},
      restart => transient,
      shutdown => 2000,
      type => worker,
      module => [erlang_socks5, socks5_statem, socks5_connections_sup, authorization_config]
    },
    #{
      id => socks5_connections_sup,
      start => {socks5_connections_sup, start_link, [InAddr, OutAddr]},
      restart => transient,
      shutdown => 2000,
      type => supervisor,
      module => [socks5_statem, socks5_connections_sup, authorization_config]
    }
  ],
  {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
