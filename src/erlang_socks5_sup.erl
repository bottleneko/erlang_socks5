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

-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

-spec init([]) -> {ok, {SupFlags, ChildSpecs}} when
    SupFlags   :: supervisor :sup_flags(),
    ChildSpecs :: [ChildSpec],
    ChildSpec  :: [supervisor:child_spec()].
init([]) ->
  SupFlags = #{
    strategy => one_for_one
  },
  ChildSpec =
    #{id       => es5_authentication_server,
      start    => {es5_authentication_server, start_link, []},
      restart  => transient,
      shutdown => 2000,
      type     => worker,
      module   => [es5_authentication_server]
     },
  {ok, {SupFlags, [ChildSpec]}}.
