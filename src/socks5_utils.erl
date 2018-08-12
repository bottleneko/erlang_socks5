-module(socks5_utils).

%% API
-export([get_interface_name/1, get_interface_address/2]).

-export([address_number/1]).
-export([to_inet/1]).
-export([address_protocol/1]).
-export([protocol_address_length/1]).
-export([protocol_address_bit_length/1]).

get_interface_name(Addr) ->
  {ok, IfList} = inet:getifaddrs(),
  IfNames = lists:foldl(
    fun({IfName, Addrs}, Acc) ->
      IfAddrs = proplists:get_all_values(addr, Addrs),
      case lists:member(Addr, IfAddrs) of
        true -> [IfName | Acc];
        false -> Acc
      end
    end, [], IfList),
  case IfNames of
    [IfName] ->
      {ok, IfName};
    _ ->
      {error, no_match_one_interface_name}
  end.

-spec get_interface_address(Protocol :: ipv4 | ipv6, InterfaceName :: string()) -> tuple().
get_interface_address(Protocol, InterfaceName) ->
  {ok, InterfacesList} = inet:getifaddrs(),
  OutInterface = proplists:get_value(InterfaceName, InterfacesList),
  OutAddresses = proplists:lookup_all(addr, OutInterface),
  ValidOutAddresses = lists:filter(
    fun
      ({addr, IOutAddress}) when size(IOutAddress) =:= 4, Protocol =:= ipv4 ->
        true;
      ({addr, IOutAddress}) when size(IOutAddress) =:= 8, Protocol =:= ipv6 ->
        true;
      (_) ->
        false
    end, OutAddresses),
  {addr, OutAddress} = hd(ValidOutAddresses),
  {ok, OutAddress}.

address_number({Oct1, Oct2, Oct3, Oct4}) ->
  B1 = Oct1 bsl 24,
  B2 = Oct2 bsl 16,
  B3 = Oct3 bsl 8,
  B4 = Oct4,
  B1 + B2 + B3 + B4;
address_number({Hex1, Hex2, Hex3, Hex4, Hex5, Hex6, Hex7, Hex8}) ->
  B1 = Hex1 bsl 112,
  B2 = Hex2 bsl 96,
  B3 = Hex3 bsl 80,
  B4 = Hex4 bsl 64,
  B5 = Hex5 bsl 48,
  B7 = Hex6 bsl 32,
  B6 = Hex7 bsl 16,
  B8 = Hex8,
  B1 + B2 + B3 + B4 + B5 + B6 + B7 + B8.

-spec to_inet
    (ipv4) -> inet;
    (ipv6) -> inet6.
to_inet(ipv4) -> inet;
to_inet(ipv6) -> inet6.

%% TODO: validate content
-spec address_protocol(tuple()) -> {ok, ipv4 | ipv6}.
address_protocol(Address) when tuple_size(Address) =:= 4 ->
  {ok, ipv4};
address_protocol(Address) when tuple_size(Address) =:= 8 ->
  {ok, ipv6};
address_protocol(_Address) ->
  {error, no_protocol}.

%% TODO: спеки запилил быстро бля

protocol_address_length(ipv4) -> 4;
protocol_address_length(ipv6) -> 8.

protocol_address_bit_length(ipv4) -> 32;
protocol_address_bit_length(ipv6) -> 128.
