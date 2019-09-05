-module(erlang_socks5_app).

-behaviour(application).

%%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec start(StartType, StartArgs) -> {ok, pid()} when
    StartType :: any(),
    StartArgs :: any().
start(_StartType, _StartArgs) ->
  ListenAddress = es5_config:listen_address(),
  ClientsAddress = es5_config:clients_address(),

  SocketOpts =
    [binary,
     {port, es5_config:listen_port()},
     {ifaddr, ListenAddress},
     {packet, raw},
     {active, false},
     {reuseaddr, true}
    ],
  {ok, _} = ranch:start_listener(erlang_socks5, ranch_tcp, #{socket_opts => SocketOpts}, es5_connection, [ListenAddress, ClientsAddress]),
  erlang_socks5_sup:start_link().

-spec stop(State :: any()) -> ok.
stop(_State) ->
  ok.
