%%%-------------------------------------------------------------------
%%% @author kim <kim@limmen>
%%% @copyright (C) 2016, kim
%%% @doc
%%% Property tests for marley_lib_http
%%% @end
%%% Created : 30 Jul 2016 by kim <kim@limmen>
%%%-------------------------------------------------------------------
-module(prop_marley_lib_http).

%% API
-compile(export_all).

%% Includes
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================
prop_parse_request()->
    ?FORALL({Method,URI,Version,Headers,Body}, {parsed_http_method(), plain_string_not_empty(), parsed_http_version(), list(header()), list()}, validate(marley_lib_http:parse_request(format_request_string(Method, URI, Version, Headers, Body)), {Method,URI,Version,Headers,Body})).

%%%===================================================================
%%% Generators
%%%===================================================================

header()->
    ?LET({X,Y}, {plain_string_not_empty(), plain_string()}, X ++ ":" ++ Y ++ "\r\n").

plain_string()->
    ?SUCHTHAT(X, list(integer()), lists:all(fun(C) -> C /= 32 andalso C /= 13 andalso C /= 10 andalso C /= 58 end, X)).

plain_string_not_empty()->
    ?SUCHTHAT(X, list(integer()), lists:all(fun(C) -> C /= 32 andalso C /= 13 andalso C /= 10 andalso C /= 58  end, X) andalso length(X) > 0).

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

%%%===================================================================
%%% Properties
%%%===================================================================

validate(Result, {Method,URI,Version,Headers,Body})->
    ?assert(is_map(Result)),
    ?assertMatch(Method,maps:get(http_method, maps:get(request_line, Result))),
    ?assertMatch(Version, maps:get(http_version, maps:get(request_line, Result))),
    ?assertMatch(URI, maps:get(http_uri, maps:get(request_line, Result))),
    ?assert(length(string:tokens(lists:flatten(Headers), "\r\n")) =:= length(maps:get(headers, Result))),
    true.

%% ?assert(lists:all(fun({Property, Value}) -> 
%%                           PropertiesAndValues = string:tokens(lists:flatten(string:tokens(Headers, "\r\n")),":"),
%%                           lists:member(Property, PropertiesAndValues) andalso lists:member(Value, PropertiesAndValues) end, maps:get(headers, Result))),

%%%===================================================================
%%% Helper functions
%%%===================================================================

format_request_string(Method, URI, Version, [], Body)->
    string:to_upper(atom_to_list(Method)) ++ " " ++ URI++ " " ++ atom_to_list(Version)  ++ "\r\n" ++ "\r\n\r\n" ++ Body;

format_request_string(Method, URI, Version, Headers, Body)->
    string:to_upper(atom_to_list(Method)) ++ " " ++ URI ++ " " ++ atom_to_list(Version)  ++ "\r\n" ++ lists:flatten(Headers) ++ "\r\n" ++ Body.
