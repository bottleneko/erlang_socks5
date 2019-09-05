-module(es5_protocol).

-include_lib("kernel/include/inet.hrl").

-include("socks5_method_codes.hrl").

-type cmd() :: connect | bind | udp_associate | unsupported.
-export_type([cmd/0]).

%%% API
-export([request_auth/0,
         success_authentication/0,
         authentication_fail/0,
         success_connect/2,
         host_not_available/2,
         command_not_available/2,
         get_destination_endpoint/1,
         get_cmd/1,
         get_credentials/1
        ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec request_auth() -> iolist().
request_auth() ->
   [<<?PROTOCOL_VERSION>>, <<?USERNAME_PASSWORD_AUTH>>].

-spec success_authentication() -> iolist().
success_authentication() ->
  <<?USERNAME_PASSWORD_AUTH_VERSION, ?USERNAME_PASSWORD_AUTH_SUCCESS>>.

-spec authentication_fail() -> iolist().
authentication_fail() ->
  <<?USERNAME_PASSWORD_AUTH_VERSION, ?USERNAME_PASSWORD_AUTH_FAIL>>.

-spec success_connect(Address, Port) -> iolist() when
    Address :: inet:ip4_address(),
    Port    :: inet:port_number().
success_connect(Address, Port) ->
  NumericAddress = inet4_octets(Address),
  <<?PROTOCOL_VERSION, ?SUCCESS_CONNECT, ?RESERVED, ?QUERY_IPv4, NumericAddress:32, Port:16>>.

-spec host_not_available(Address, Port) -> iolist() when
    Address :: inet:ip4_address(),
    Port    :: inet:port_number().
host_not_available(Address, Port) ->
  NumericAddress = inet4_octets(Address),
  <<?PROTOCOL_VERSION, ?HOST_NOT_AVAIL, ?RESERVED, ?QUERY_IPv4, NumericAddress:32, Port:16>>.

-spec command_not_available(Address, Port) -> iolist() when
    Address :: inet:ip4_address(),
    Port    :: inet:port_number().
command_not_available(Address, Port) ->
  NumericAddress = inet4_octets(Address),
  <<?PROTOCOL_VERSION, ?COMMAND_NOT_AVAIL, ?RESERVED, ?QUERY_IPv4, NumericAddress:32, Port:16>>.

-spec get_destination_endpoint(Request) -> {Address, Port} when
    Request :: binary(),
    Address :: inet:ip4_address(),
    Port    :: inet:port_number().
get_destination_endpoint(Request) ->
  ATyp = binary:at(Request, 3),
  case ATyp of
    ?QUERY_IPv4 ->
      <<?PROTOCOL_VERSION, _Cmd, ?RESERVED, ATyp, DstAddrBin:4/binary, Port:16>> = Request,
      DstAddr = list_to_tuple([ X || <<X>> <= DstAddrBin ]),
      {DstAddr, Port};
    ?QUERY_FQDN ->
      <<?PROTOCOL_VERSION, _Cmd, ?RESERVED, ATyp, DLen, Dom:DLen/binary, Port:16>> = Request,
      DstAddr =
        case inet:gethostbyname(binary_to_list(Dom)) of
          {ok, Hostent} ->
            hd(Hostent#hostent.h_addr_list)
        end,
      {DstAddr, Port};
    ?QUERY_IPv6 ->
      <<?PROTOCOL_VERSION, _Cmd, ?RESERVED, ATyp, DstAddrBin:16/binary, Port:16>> = Request,
      DstAddr = list_to_tuple([ X || <<X>> <= DstAddrBin ]),
      {DstAddr, Port}
  end.

-spec get_cmd(Request) -> Cmd when
    Request :: binary(),
    Cmd     :: cmd().
get_cmd(Request) ->
  case binary:at(Request, 1) of
    ?CONNECT_CMD ->
      connect;
    ?BIND_CMD ->
      bind;
    ?UDP_ASSOC_CMD ->
      udp_associate;
    _ ->
      unsupported
  end.

-spec get_credentials(Request) -> {Username, Password} when
    Request  :: binary(),
    Username :: binary(),
    Password :: binary().
get_credentials(<<1, ULen, Username:ULen/binary, PLen, Password:PLen/binary>> = _Request) ->
  {Username, Password}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

inet4_octets({Oct1, Oct2, Oct3, Oct4}) ->
  B1 = Oct1 bsl 24,
  B2 = Oct2 bsl 16,
  B3 = Oct3 bsl 8,
  B4 = Oct4,
  B1 + B2 + B3 + B4.
