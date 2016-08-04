%%%-------------------------------------------------------------------
%%% @author Kim Hammar <kimham@kth.se>
%%% @copyright (C) 2016, Kim Hammar
%%% @doc
%%% Property tests for marley_http
%%% @end
%%% Created : 30 Jul 2016 by Kim Hammar <kimham@kth.se>
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
    ?FORALL({Method,URI,Version,Headers,Body}, {marley_http:parsed_http_method(), plain_string_not_empty(), marley_http:parsed_http_version(), list(header()), list(char())}, validate_parsed_request(marley_http:parse_request((format_request_string(Method, URI, Version, Headers, Body))), {Method,URI,Version,Headers,Body})).

prop_http_response()->
    ?FORALL({Version, Code, Body,Headers}, {marley_http:parsed_http_version(), http_code(), binary(), binary()}, validate_http_response(marley_http:http_response(Version, Code, Body, Headers))).

%%%===================================================================
%%% Generators
%%%===================================================================

header()->
    ?LET({X,Y}, {plain_string_not_empty(), plain_string()}, X ++ ":" ++ Y ++ "\r\n").

plain_string()->
    ?SUCHTHAT(X, list(char()), lists:all(fun(C) -> C /= 32 andalso C /= 13 andalso C /= 10 andalso C /= 58 end, X)).

plain_string_not_empty()->
    ?SUCHTHAT(X, list(char()), lists:all(fun(C) -> C /= 32 andalso C /= 13 andalso C /= 10 andalso C /= 58 end, X) andalso length(X) > 0).

http_code()->
    Codes = [100,101,102,200,201,202,203,204,205,206,207,226,300,301,302,
             303,304,305,306,307,400,401,402,403,404,405,406,407,408,409,
             410,411,412,413,414,415,416,417,418,422,423,424,425,426,428,
             429,431,500,501,502,503,504,505,506,507,510,511],
    ?SUCHTHAT(X, integer(99,512), lists:member(X,Codes)).

%%%===================================================================
%%% Properties
%%%===================================================================

validate_parsed_request(Result, {Method,URI,Version,Headers,Body})->
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

validate_http_response(Result) ->
    ?assert(is_binary(Result)),
    true.

%%%===================================================================
%%% Helper functions
%%%===================================================================

format_request_string(Method, URI, Version, [], Body)->
    unicode:characters_to_binary(string:to_upper(atom_to_list(Method)) ++ " " ++ URI++ " " ++ atom_to_list(Version)  ++ "\r\n" ++ "\r\n\r\n" ++ Body);

format_request_string(Method, URI, Version, Headers, Body)->
    unicode:characters_to_binary(string:to_upper(atom_to_list(Method)) ++ " " ++ URI ++ " " ++ atom_to_list(Version)  ++ "\r\n" ++ lists:flatten(Headers) ++ "\r\n" ++ Body).
