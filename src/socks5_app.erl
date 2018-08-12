%%%-------------------------------------------------------------------
%% @doc socks5 public API
%% @end
%%%-------------------------------------------------------------------

-module(socks5_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  {ok, SupPid} = socks5_sup:start_link(),
  {ok, Configs} = socks5_config:get_configs(),
  lists:foreach(
    fun(Config) ->
      Interface = socks5_config:get_interface_name(Config),
      ProtocolsConfig = socks5_config:get_protocols_config(Config),

      lists:foreach(
        fun(ProtocolConfig) ->
          Protocol = socks5_config:get_protocol(ProtocolConfig),
          ListensConfig = socks5_config:get_listens_config(ProtocolConfig),

          lists:foreach(
            fun(ListenConfig) ->
              ListenPort = socks5_config:get_port(ListenConfig),
              RoutesConfig = socks5_config:get_routes_config(ListenConfig),

              RouteDestinations = lists:map(fun socks5_config:get_route_destination/1, RoutesConfig),

              %% FIXME: route args
              {ok, _Pid} = supervisor:start_child(
                socks5_listener_sup,
                [{{Interface, Protocol, ListenPort}, RouteDestinations}]
              )
            end, ListensConfig)
        end, ProtocolsConfig)
    end, Configs),
  {ok, SupPid}.

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
