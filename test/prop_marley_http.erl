%%%-------------------------------------------------------------------
%%% @author kim <kim@limmen>
%%% @copyright (C) 2016, kim
%%% @doc
%%% Property tests for marley_http
%%% @end
%%% Created : 30 Jul 2016 by kim <kim@limmen>
%%%-------------------------------------------------------------------
-module(prop_marley_http).

%% API
-compile(export_all).

%% Includes
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================

prop_parse_request()->
    ?FORALL({Method,URI,Version,Headers,Body}, {marley_http:parsed_http_method(), plain_string_not_empty(), marley_http:parsed_http_version(), list(header()), list(char())}, validate(marley_http:parse_request((format_request_string(Method, URI, Version, Headers, Body))), {Method,URI,Version,Headers,Body})).

%%%===================================================================
%%% Generators
%%%===================================================================

header()->
    ?LET({X,Y}, {plain_string_not_empty(), plain_string()}, X ++ ":" ++ Y ++ "\r\n").

plain_string()->
    ?SUCHTHAT(X, list(char()), lists:all(fun(C) -> C /= 32 andalso C /= 13 andalso C /= 10 andalso C /= 58 end, X)).

plain_string_not_empty()->
    ?SUCHTHAT(X, list(char()), lists:all(fun(C) -> C /= 32 andalso C /= 13 andalso C /= 10 andalso C /= 58 end, X) andalso length(X) > 0).

%%%===================================================================
%%% Properties
%%%===================================================================

validate(Result, {Method,URI,Version,Headers,Body})->
    BinURI = unicode:characters_to_binary(URI),
    BinBody = unicode:characters_to_binary(Body),
    ?assert(is_map(Result)),
    ?assert(maps:size(Result) =:= 3),
    ?assert(maps:is_key(body, Result)),
    ?assert(maps:is_key(headers, Result)),
    ?assert(maps:is_key(request_line, Result)),
    ?assert(maps:size(maps:get(request_line, Result)) =:= 3),
    ?assert(maps:is_key(http_method, maps:get(request_line, Result))),
    ?assert(maps:is_key(http_uri, maps:get(request_line, Result))),
    ?assert(maps:is_key(http_version, maps:get(request_line, Result))),
    ?assertMatch(Method,maps:get(http_method, maps:get(request_line, Result))),
    ?assertMatch(Version, maps:get(http_version, maps:get(request_line, Result))),
    ?assertMatch(BinURI, maps:get(http_uri, maps:get(request_line, Result))),
    ?assert(length(string:tokens(lists:flatten(Headers), "\r\n")) =:= length(maps:get(headers, Result))),
    ?assertMatch(BinBody, maps:get(body, Result)),
    true.

%%%===================================================================
%%% Helper functions
%%%===================================================================

format_request_string(Method, URI, Version, [], Body)->
    unicode:characters_to_binary(string:to_upper(atom_to_list(Method)) ++ " " ++ URI++ " " ++ atom_to_list(Version)  ++ "\r\n" ++ "\r\n\r\n" ++ Body);

format_request_string(Method, URI, Version, Headers, Body)->
    unicode:characters_to_binary(string:to_upper(atom_to_list(Method)) ++ " " ++ URI ++ " " ++ atom_to_list(Version)  ++ "\r\n" ++ lists:flatten(Headers) ++ "\r\n" ++ Body).
