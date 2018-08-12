-module(socks5_config).

-type interface_name() :: string().

-type protocol() :: ipv4 | ipv6.
-type socket_port() :: non_neg_integer().

-type route_source() :: {protocol(), interface_name(), socket_port()}.
-type route_destination() :: {protocol(), interface_name()}.
-type route() :: {route_source(), [route_destination()]}.

-export_type([route/0]).

-type account() :: {binary(), binary()}.

-export_type([account/0]).

-opaque param() :: {atom(), term()}.

-opaque route_config() :: {interface_name(), protocol(), [param()]}.
-opaque listen_config() :: {socket_port(), [route_config()], [param()]}.
-opaque protocol_config() :: {protocol(), [listen_config()], [param()]}.
-opaque interface_config() :: {interface_name(), [protocol_config()], [param()]}.

-export_type([route_config/0, listen_config/0, interface_config/0, interface_config/0]).

-type config() :: interface_config() | protocol_config() | listen_config() | route_config().

-export_type([config/0]).
%% TODO: ADD GEN CONFIG TYPE

%% API
-export([get_configs/0]).

-export([get_config_params/1]).

-export([by_interface_name/1]).
-export([get_interface_name/1]).
-export([get_protocols_config/1]).
-export([get_protocol_config/2]).

-export([get_protocol/1]).
-export([get_listens_config/1]).
-export([get_listen_config/2]).

-export([get_port/1]).
-export([get_routes_config/1]).

-export([get_out_interface/1]).

-export([get_route_destination/1]).

-export([get_config_authentication_credentials/1]).

-spec get_configs() ->
  {ok, [interface_config()]} | {error, undefined}.
get_configs() ->
  application:get_env(socks5, binded_interfaces).

%%%===================================================================
%%% Config-level getters
%%%===================================================================

-spec by_interface_name(InterfaceName :: interface_name()) ->
  {ok, interface_config()} | {error, undefined}.
by_interface_name(InterfaceName) ->
  {ok, Interfaces} = application:get_env(socks5, binded_interfaces),
  Config = lists:keyfind(InterfaceName, 1, Interfaces),
  case Config of
    false ->
      {error, undefined};
    Config ->
      {ok, Config}
  end.

-spec get_interface_name(Config :: interface_config()) ->
  interface_name().
get_interface_name({InterfaceName, _InterfaceConfig, _Params} = _Config) ->
  InterfaceName.

-spec get_config_params(Config :: config()) ->
  [param()].
get_config_params({_Key, _SubConfigs, Params} = _Config) ->
  Params.

-spec get_protocols_config(Config :: interface_config()) ->
  [protocol_config()].
get_protocols_config({_InterfaceName, ProtocolsConfig, _Params} = _Config) ->
  ProtocolsConfig.

-spec get_protocol_config(Protocol :: protocol(), Config :: interface_config()) ->
  {ok, protocol_config()}.
get_protocol_config(Protocol, {_InterfaceName, ProtocolsConfig, _Params} = _Config) ->
  InterfaceConfig = proplists:get_value(Protocol, ProtocolsConfig),
  case InterfaceConfig of
    undefined ->
      {error, undefined};
    Protocol ->
      {ok, Protocol}
  end.

%%%===================================================================
%%% Protocol_config-level getters
%%%===================================================================

-spec get_protocol(Config :: interface_config()) ->
  protocol().
get_protocol({Protocol, _ListenConfig, _Params} = _Config) ->
  Protocol.

-spec get_listens_config(InterfaceConfig :: interface_config()) ->
  [listen_config()].
get_listens_config({_Protocol, ListensConfig, _Params} = _InterfaceConfig) ->
  ListensConfig.

-spec get_listen_config(ListenPort :: socket_port(), InterfaceConfig :: interface_config()) ->
  {ok, listen_config()}.
get_listen_config(ListenPort, {_Protocol, ListensConfig, _Params} = _InterfaceConfig) ->
  ListenConfig = proplists:get_value(ListenPort, ListensConfig),
  case ListenConfig of
    undefined ->
      {error, undefined};
    ListenConfig ->
      {ok, ListenConfig}
  end.

%%%===================================================================
%%% Listen_config-level getters
%%%===================================================================

-spec get_port(ListenConfig :: listen_config()) ->
  socket_port().
get_port({Port, _RoutesConfig, _Params} = _ListenConfig) ->
  Port.

-spec get_routes_config(ListenConfig :: listen_config()) ->
  [route_config()].
get_routes_config({_Port, RoutesConfig, _Params} = _ListenConfig) ->
  RoutesConfig.

%%%===================================================================
%%% Router_config-level getters
%%%===================================================================

-spec get_out_interface(RouteConfig :: route_config()) ->
  interface_name().
get_out_interface({Interface, _Protocol, _Params} = _RouteConfig) ->
  Interface.

-spec get_route_destination(RouteConfig :: route_config()) ->
  route_destination().
get_route_destination({Interface, Protocol, _Params} = _RouteConfig) ->
  {Interface, Protocol}.

-spec get_config_authentication_credentials(Config :: [param()]) ->
  {ok, [account()]} | {error, undefined}.
get_config_authentication_credentials(Params) ->
  Authentication = proplists:get_value(authentication, Params),
  AccountStorage = proplists:get_value(account_storage, Authentication),
  case proplists:get_value(config, AccountStorage) of
    undefined ->
      {error, undefined};
    Accounts ->
      {ok, Accounts}
  end.