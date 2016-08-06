%%%-------------------------------------------------------------------
%%% @author Kim Hammar <kimham@kth.se>
%%% @copyright (C) 2016, Kim Hammar
%%% @doc
%%% Suite of functions for parsing HTTP requests and responses
%%% @end
%%% Created : 30 Jul 2016 by Kim Hammar <kimham@kth.se>
%%%-------------------------------------------------------------------
-module(marley_http).

%% API
-export([parse_request/1, http_response/4, status/1]).
-export_type([parsed_http_method/0, parsed_http_version/0, parsed_http_request/0]).

%% Types
-type parsed_http_request():: #{
                           request_line => parsed_http_request_line(),
                           headers => [parsed_http_header()],
                           body => binary()
                          }.

-type parsed_http_request_line():: #{
                                http_method => parsed_http_method(),
                                http_uri => binary(),
                                http_version => parsed_http_version()
                               }.

-type parsed_http_method():: get
                           | post
                           | head
                           | put
                           | delete
                           | trace
                           | options
                           | connect
                           | patch.


-type parsed_http_version():: 'HTTP/1.0'
                            | 'HTTP/1.1'.

-type parsed_http_header():: {binary(), binary()}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Parses a HTTP request in text-form into a parsed_http_request()
%%
%% @spec parse_request(Req) -> ParsedRequest
%% @end
%%--------------------------------------------------------------------
-spec parse_request(binary()) -> parsed_http_request().
parse_request(Req)->
    {RequestLine, R0} = parse_request_line(remove_leading_crlf(Req)),
    {Headers, Body} = parse_headers(R0),
    #{request_line => RequestLine, headers => Headers, body => Body}.

%%--------------------------------------------------------------------
%% @doc
%% Constructs a HTTP response given a set of parameters.
%%
%% @spec http_response(Version, Code, Body, UserHeaders) -> HTTPResponse
%% @end
%%--------------------------------------------------------------------
-spec http_response(parsed_http_version(), integer(), binary(), binary()) -> binary().
http_response(Version, Code, Body, UserHeaders)->
    BinVer = atom_to_binary(Version, unicode),
    Status = status(Code),
    Headers = default_headers(Body, UserHeaders),
    <<BinVer/bits, 32, Status/bits,
      "\r\n", Headers/bits, Body/bits>>.



%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Parses the HTTP Request Line
%%
%% @spec parse_request_line(Req) -> {ParsedRequestLine, RestOfRequest}
%% @end
%%--------------------------------------------------------------------
-spec parse_request_line(binary()) -> {parsed_http_request_line(), binary()}.
parse_request_line(Req) ->
    {Method, R0} = parse_method(Req),
    {URI, R1} = parse_uri(R0),
    {Version, R2} = parse_version(R1),
    <<13, 10, R3/bits>> = R2,
    {#{http_method => Method, http_uri => URI, http_version => Version}, R3}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Parses HTTP URI
%%
%% @spec parse_uri(Req) -> {ParsedURI, RestOfRequest}
%% @end
%%--------------------------------------------------------------------
-spec parse_uri(binary()) -> {binary(), binary()}.
parse_uri(Req) ->
    parse_uri(Req, <<>>).

-spec parse_uri(binary(), binary()) -> {binary(), binary()}.
parse_uri(<<32, R1/bits>>, URI)->
    {URI, R1};
parse_uri(<<X, R1/bits>>, SoFar)->
    parse_uri(R1, <<SoFar/binary, X>>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Parses HTTP Method
%%
%% @spec parse_method(Req) -> {ParsedMethod, RestOfRequest}
%% @end
%%--------------------------------------------------------------------
-spec parse_method(binary()) -> {parsed_http_method(), binary()}.
parse_method(Req)->
    parse_method(Req, <<>>).

-spec parse_method(binary(), binary()) -> {parsed_http_method(), binary()}.
parse_method(<<32, R0/bits>>, SoFar)->
    {list_to_atom(string:to_lower(binary_to_list(SoFar))), R0};

parse_method(<<X, R0/bits>>, SoFar)->
    parse_method(R0, <<SoFar/binary, X>>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Parses HTTP Version
%%
%% @spec parse_version(Req) -> {ParsedVersion, RestOfRequest}
%% @end
%%--------------------------------------------------------------------
-spec parse_version(binary()) -> {parsed_http_version(), binary()}.
parse_version(<<$H, $T, $T, $P, $/, $1, $., $1, R0/bits>>) ->
    {'HTTP/1.1', R0};

parse_version(<<$H, $T, $T, $P, $/, $1, $., $0, R0/bits>>) ->
    {'HTTP/1.0', R0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Parses HTTP Headers
%%
%% @spec parse_headers(Req) -> {Parsedheaders, RestOfRequest}
%% @end
%%--------------------------------------------------------------------
-spec parse_headers(binary()) -> {[parsed_http_header()], binary()}.
parse_headers(Req)->
    {Headers, R1} = get_headers(Req, <<>>),
    ParsedHeaders = parse_headers(string:tokens(Headers, "\r\n"), []),
    {ParsedHeaders, R1}.

parse_headers([], SoFar)->
    lists:reverse(SoFar);

parse_headers([H|T], SoFar)->
    [Prop|Val] = string:tokens(H, ":"),
    parse_headers(T, [{Prop, Val}|SoFar]).

%%%===================================================================
%%% Helper functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Extracts the headers part from the request
%%
%% @spec get_headers(Req, SoFar) -> {Headers, RestOfRequest}
%% @end
%%--------------------------------------------------------------------
-spec get_headers(binary(), binary()) -> {list(), binary()}.
get_headers(<<$\r, $\n, $\r, $\n, R1/bits>>, Headers)->
    {binary_to_list(Headers), R1};

get_headers(<<H, T/bits>>, SoFar)->
    get_headers(T, <<SoFar/binary, H>>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Removes leading CRLF from request
%%
%% @spec remove_leading_crlf(Req) -> ReqWithoutLeadingCRLF
%% @end
%%--------------------------------------------------------------------
-spec remove_leading_crlf(binary()) -> binary().
remove_leading_crlf(<<$\r, $\n, T/bits>>)->
    remove_leading_crlf(T);
remove_leading_crlf(Req) ->
    Req.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Adds default response headers: Connection and Content-Length to
%% user defined headers.
%%
%% @spec default_headers(Body, UserHeaders) -> Headers
%% @end
%%--------------------------------------------------------------------
-spec default_headers(binary(), binary() | list()) -> binary().
default_headers(Body, Headers) when is_binary(Body)->
    Size = integer_to_binary(byte_size(Body)),
    <<"Connection: Keep-Alive\r\n", "Content-Length:",
      Size/bits, "\r\n", Headers/bits, "\r\n">>;

default_headers(Body, Headers) when is_list(Body)->
    Size = integer_to_binary(length(Body)),
    <<"Connection: Keep-Alive\r\n", "Content-Length:",
      Size/bits, "\r\n", Headers/bits, "\r\n">>.


%%--------------------------------------------------------------------
%% @doc
%% HTTP status codes.
%% Response code string. Lifted from cowboy_http_req.erl
%%
%% @spec status(Code) -> StatusString
%% @end
%%--------------------------------------------------------------------
-spec status(integer()) -> binary().
status(100) -> <<"100 Continue">>;
status(101) -> <<"101 Switching Protocols">>;
status(102) -> <<"102 Processing">>;
status(200) -> <<"200 OK">>;
status(201) -> <<"201 Created">>;
status(202) -> <<"202 Accepted">>;
status(203) -> <<"203 Non-Authoritative Information">>;
status(204) -> <<"204 No Content">>;
status(205) -> <<"205 Reset Content">>;
status(206) -> <<"206 Partial Content">>;
status(207) -> <<"207 Multi-Status">>;
status(226) -> <<"226 IM Used">>;
status(300) -> <<"300 Multiple Choices">>;
status(301) -> <<"301 Moved Permanently">>;
status(302) -> <<"302 Found">>;
status(303) -> <<"303 See Other">>;
status(304) -> <<"304 Not Modified">>;
status(305) -> <<"305 Use Proxy">>;
status(306) -> <<"306 Switch Proxy">>;
status(307) -> <<"307 Temporary Redirect">>;
status(400) -> <<"400 Bad Request">>;
status(401) -> <<"401 Unauthorized">>;
status(402) -> <<"402 Payment Required">>;
status(403) -> <<"403 Forbidden">>;
status(404) -> <<"404 Not Found">>;
status(405) -> <<"405 Method Not Allowed">>;
status(406) -> <<"406 Not Acceptable">>;
status(407) -> <<"407 Proxy Authentication Required">>;
status(408) -> <<"408 Request Timeout">>;
status(409) -> <<"409 Conflict">>;
status(410) -> <<"410 Gone">>;
status(411) -> <<"411 Length Required">>;
status(412) -> <<"412 Precondition Failed">>;
status(413) -> <<"413 Request Entity Too Large">>;
status(414) -> <<"414 Request-URI Too Long">>;
status(415) -> <<"415 Unsupported Media Type">>;
status(416) -> <<"416 Requested Range Not Satisfiable">>;
status(417) -> <<"417 Expectation Failed">>;
status(418) -> <<"418 I'm a teapot">>;
status(422) -> <<"422 Unprocessable Entity">>;
status(423) -> <<"423 Locked">>;
status(424) -> <<"424 Failed Dependency">>;
status(425) -> <<"425 Unordered Collection">>;
status(426) -> <<"426 Upgrade Required">>;
status(428) -> <<"428 Precondition Required">>;
status(429) -> <<"429 Too Many Requests">>;
status(431) -> <<"431 Request Header Fields Too Large">>;
status(500) -> <<"500 Internal Server Error">>;
status(501) -> <<"501 Not Implemented">>;
status(502) -> <<"502 Bad Gateway">>;
status(503) -> <<"503 Service Unavailable">>;
status(504) -> <<"504 Gateway Timeout">>;
status(505) -> <<"505 HTTP Version Not Supported">>;
status(506) -> <<"506 Variant Also Negotiates">>;
status(507) -> <<"507 Insufficient Storage">>;
status(510) -> <<"510 Not Extended">>;
status(511) -> <<"511 Network Authentication Required">>.

