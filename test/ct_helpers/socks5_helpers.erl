-module(socks5_helpers).

-include("socks5_method_codes.hrl").

%% API
-export([make_connect_to_remote_host/3]).
-export([make_udp_assoc_to_remote_host/3]).

make_connect_to_remote_host(Socket, Domain, Port) when is_binary(Domain) ->
  gen_tcp:send(Socket, <<
    ?PROTOCOL_VERSION,
    ?CONNECT_CMD,
    ?RESERVED,
    ?QUERY_FQDN,
    (byte_size(Domain)),
    Domain/binary,
    Port:16>>),

  {ok, ResponsePacket} = gen_tcp:recv(Socket, 10, 2000),
  <<?PROTOCOL_VERSION, Rep, ?RESERVED, ?QUERY_IPv4, _DstAddr:32, _DstPort:16>> = ResponsePacket,

  case Rep of
    0 ->
      Socket
  end;
make_connect_to_remote_host(Socket, Ipv4, Port) when size(Ipv4) == 4 ->
  gen_tcp:send(Socket, <<
    ?PROTOCOL_VERSION,
    ?CONNECT_CMD,
    ?RESERVED,
    ?QUERY_IPv4,
    (socks5_utils:address_number(Ipv4)):32,
    Port:16
  >>),

  {ok, RequestPacket} = gen_tcp:recv(Socket, 10, 2000),
  <<?PROTOCOL_VERSION, Rep, ?RESERVED, ?QUERY_IPv4, _DstAddr:32, _DstPort:16>> = RequestPacket,

  case Rep of
    0 ->
      Socket
  end;
make_connect_to_remote_host(Socket, Ipv6, Port) when size(Ipv6) == 8 ->
  gen_tcp:send(Socket, <<
    ?PROTOCOL_VERSION,
    ?CONNECT_CMD,
    ?RESERVED,
    ?QUERY_IPv6,
    (socks5_utils:address_number(Ipv6)):128,
    Port:16
  >>),

  {ok, RequestPacket} = gen_tcp:recv(Socket, 10, 2000),
  Rep =
    case RequestPacket of
      <<?PROTOCOL_VERSION, InnerRep, ?RESERVED, ?QUERY_IPv4, _DstAddr:32, _DstPort:16>> ->
        InnerRep;
      <<?PROTOCOL_VERSION, InnerRep, ?RESERVED, ?QUERY_IPv6, _DstAddr:128, _DstPort:16>> ->
        InnerRep
    end,

  case Rep of
    0 ->
      Socket
  end.


-include_lib("eunit/include/eunit.hrl").

make_udp_assoc_to_remote_host(Socket, Ipv4, Port) when size(Ipv4) == 4 ->
  gen_tcp:send(Socket, <<
    ?PROTOCOL_VERSION,
    ?UDP_ASSOC_CMD,
    ?RESERVED,
    ?QUERY_IPv4,
    (socks5_utils:address_number(Ipv4)):32,
    Port:16
  >>),

  {ok, ResponsePacket} = gen_tcp:recv(Socket, 10, 2000),
  <<?PROTOCOL_VERSION, Rep, ?RESERVED, ?QUERY_IPv4, _DstAddr:32, DstPort:16>> = ResponsePacket,

  case Rep of
    0 ->
      {{127,0,0,1}, DstPort}
  end;
make_udp_assoc_to_remote_host(Socket, Ipv6, Port) when size(Ipv6) == 8 ->
  gen_tcp:send(Socket, <<
    ?PROTOCOL_VERSION,
    ?UDP_ASSOC_CMD,
    ?RESERVED,
    ?QUERY_IPv6,
    (socks5_utils:address_number(Ipv6)):128,
    Port:16
  >>),
  {ok, ResponsePacket} = gen_tcp:recv(Socket, 0, 2000),

  case ResponsePacket of
    <<?PROTOCOL_VERSION, 0, ?RESERVED, ?QUERY_IPv4, _DstAddr:32, DstPort:16>> ->
      {{127,0,0,1}, DstPort};
    <<?PROTOCOL_VERSION, 0, ?RESERVED, ?QUERY_IPv6, _DstAddr:128, DstPort:16>> ->
      {{0,0,0,0,0,0,0,1}, DstPort}
  end.