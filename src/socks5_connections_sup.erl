
-module(socks5_connections_sup).
-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec(start_link(InAddr :: tuple(), OutAddr :: tuple()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(InAddr, OutAddr) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [{in_interface_address, InAddr}, {out_interface_address, OutAddr}]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([{in_interface_address, InAddr}, {out_interface_address, OutAddr}]) ->
  SupFlags = #{
    strategy => simple_one_for_one
  },
  ChildSpecs = [#{
    id => es5_statem,
    start => {es5_statem, start_link, [InAddr, OutAddr]},
    restart => temporary,
    shutdown => 2000,
    type => worker,
    modiles => [es5_statem]
  }],
  {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
