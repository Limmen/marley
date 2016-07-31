%%%-------------------------------------------------------------------
%%% @author kim <kim@limmen>
%%% @copyright (C) 2016, kim
%%% @doc
%%% Suite of functions for parsing HTTP requests and responses
%%% @end
%%% Created : 30 Jul 2016 by kim <kim@limmen>
%%%-------------------------------------------------------------------
-module(marley_http).

%% API
-export([parse_request/1]).
-export_type([parsed_http_method/0, parsed_http_version/0]).

%% Types
-type parsed_http_request():: #{
                           request_line => parsed_http_request_line(),
                           headers => [parsed_http_header()],
                           body => list()
                          }.

-type parsed_http_request_line():: #{
                                http_method => parsed_http_method(),
                                http_uri => list(),
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

-type parsed_http_header():: {list(), list()}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Parses a HTTP request in text-form
%% @end
%%--------------------------------------------------------------------
-spec parse_request(list()) -> parsed_http_request().
parse_request(Req)->
    {RequestLine, R0} = parse_request_line(remove_leading_crlf(Req)),
    {Headers, Body} = parse_headers(R0),
    #{request_line => RequestLine, headers => Headers, body => Body}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%%
%% Functions to parse a HTTP request
%%
%%--------------------------------------------------------------------

%% Parses request line
-spec parse_request_line(list()) -> {parsed_http_request_line(), list()}.
parse_request_line(Req) ->
    {Method, R0} = parse_method(Req),
    {URI, R1} = parse_uri(R0),
    {Version, R2} = parse_version(R1),
    [13, 10|R3] = R2,
    {#{http_method => Method, http_uri => URI, http_version => Version}, R3}.

%% Parses URI
-spec parse_uri(list()) -> {list(), list()}.
parse_uri(Req) ->
    parse_uri(Req, []).

-spec parse_uri(list(), list()) -> {list(), list()}.
parse_uri([32|R1], URI)->
    {lists:reverse(URI), R1};
parse_uri([X|R1], SoFar)->
    parse_uri(R1, [X|SoFar]).

%% Parses HTTP Method
-spec parse_method(list()) -> {parsed_http_method(), list()}.
parse_method(Req)->
    parse_method(Req, []).

-spec parse_method(list(), list()) -> {parsed_http_method(), list()}.
parse_method([32|R0], SoFar)->
    {list_to_atom(string:to_lower(lists:reverse(SoFar))), R0};

parse_method([X|R0], SoFar)->
    parse_method(R0, [X|SoFar]).

%% Parses HTTP Version
-spec parse_version(list()) -> {parsed_http_version(), list()}.
parse_version([$H, $T, $T, $P, $/, $1, $., $1 | R0]) ->
    {'HTTP/1.1', R0};

parse_version([$H, $T, $T, $P, $/, $1, $., $0 | R0]) ->
    {'HTTP/1.0', R0}.

%% Parses HTTP Headers
-spec parse_headers(list()) -> {[parsed_http_header()], list()}.
parse_headers(Req)->
    {Headers, R1} = get_headers(Req, []),
    ParsedHeaders = parse_headers(string:tokens(Headers, "\r\n"), []),
    {ParsedHeaders, R1}.

parse_headers([], SoFar)->
    lists:reverse(SoFar);

parse_headers([H|T], SoFar)->
    [Prop|Val] = string:tokens(H, ":"),
    parse_headers(T, [{Prop, Val}|SoFar]).

%%--------------------------------------------------------------------
%%
%% Functions to construct a HTTP Response
%%
%%--------------------------------------------------------------------

%%%===================================================================
%%% Helper functions
%%%===================================================================

%% Extracts the headers part
-spec get_headers(list(), list()) -> {list(), list()}.
get_headers([$\r, $\n, $\r, $\n|R1], Headers)->
    {lists:reverse(Headers), R1};

get_headers([H|T], SoFar)->
    get_headers(T, [H|SoFar]).

%% Removes leading CRLF from request
remove_leading_crlf([$\r, $\n|T])->
    remove_leading_crlf(T);
remove_leading_crlf(Req) ->
    Req.

