%%%-------------------------------------------------------------------
%% @doc erlang_socks5 public API
%% @end
%%%-------------------------------------------------------------------

-module(erlang_socks5_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  Port = application:get_env(erlang_socks5, proxy_port, 1080),
  {ok, InInterfaceName} = application:get_env(proxy_in_interface),
  {ok, OutInterfaceName} = application:get_env(proxy_out_interface),
  InAddr = get_ipv4_addr(InInterfaceName),
  OutAddr = get_ipv4_addr(OutInterfaceName),
  erlang_socks5_sup:start_link(Port, InAddr, OutAddr).

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================

get_ipv4_addr(InterfaceName) ->
  {ok, Interfaces} = inet:getifaddrs(),
  Interface = proplists:get_value(InterfaceName, Interfaces),
  {addr, Addr} =
    case proplists:lookup_all(addr, Interface) of
      [] ->
        throw(["No ipv4 addrs", InterfaceName]);
      Addrs ->
        Ipv4Addrs = lists:filter(
          fun({addr, Addr}) ->
            size(Addr) == 4
          end, Addrs),
        hd(Ipv4Addrs)
    end,
  Addr.