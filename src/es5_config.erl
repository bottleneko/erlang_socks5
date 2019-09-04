-module(es5_config).

-define(DEFAULT_LISTEN_PORT, 1080).

%%% API
-export([listen_port/0,
         listen_address/0,
         clients_address/0
        ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec listen_port() -> non_neg_integer().
listen_port() ->
  application:get_env(erlang_socks5, proxy_port, ?DEFAULT_LISTEN_PORT).

-spec listen_address() -> inet:ip4_address().
listen_address() ->
  {ok, InInterfaceName} = application:get_env(proxy_in_interface),
  get_ipv4_address(InInterfaceName).

-spec clients_address() -> inet:ip4_address().
clients_address() ->
  {ok, OutInterfaceName} = application:get_env(proxy_out_interface),
  get_ipv4_address(OutInterfaceName).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @FIXME: better exception and able to specify address
%% @throws ["No ipv4 addrs", InterfaceName :: string()]
-spec get_ipv4_address(InterfaceName) -> Ip4Address when
    InterfaceName :: string(),
    Ip4Address    :: inet:ip4_address().
get_ipv4_address(InterfaceName) ->
  {ok, Interfaces} = inet:getifaddrs(),
  Interface = proplists:get_value(InterfaceName, Interfaces),
  {addr, Address} =
    case proplists:lookup_all(addr, Interface) of
      [] ->
        throw(["No ipv4 addrs", InterfaceName]);
      Addresss ->
        Ipv4Addresss = lists:filter(
          fun({addr, Address}) ->
            size(Address) == 4
          end, Addresss),
        hd(Ipv4Addresss)
    end,
  Address.
