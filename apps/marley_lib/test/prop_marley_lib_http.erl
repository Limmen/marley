%%%-------------------------------------------------------------------
%%% @author kim <kim@limmen>
%%% @copyright (C) 2016, kim
%%% @doc
%%% 
%%% @end
%%% Created : 30 Jul 2016 by kim <kim@limmen>
%%%-------------------------------------------------------------------
-module(prop_marley_lib_http).

%% API
-compile(export_all).

%% Includes
-include_lib("proper/include/proper.hrl").

%% Macros

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
%%% Tests
%%%===================================================================
prop_parse_request()->
    ?FORALL({Method,URI,Version,Headers,Body}, {parsed_http_method(), plain_string_not_empty(), parsed_http_version(), list(header()), list()}, test(format_request_string(Method, URI, Version, Headers, Body))).

%%%===================================================================
%%% Generators
%%%===================================================================

header()->
    ?LET({X,Y}, {plain_string_not_empty(), plain_string()}, X ++ ":" ++ Y ++ "\r\n").

plain_string()->
    ?SUCHTHAT(X, list(), lists:all(fun(C) -> C /= 32 andalso C /= 13 andalso C /= 10 end, X)).

plain_string_not_empty()->
    ?SUCHTHAT(X, list(), lists:all(fun(C) -> C /= 32 andalso C /= 13 andalso C /= 10 end, X) andalso length(X) > 0).

%%%===================================================================
%%% Properties
%%%===================================================================

test(X)->
    io:format("res: ~p ~n", [marley_lib_http:parse_request(X)]),
    %%io:format("Generator produced: ~p ~n", [X]),
    true.


%%%===================================================================
%%% Helper functions
%%%===================================================================

format_request_string(Method, URI, Version, [], Body)->
    string:to_upper(atom_to_list(Method)) ++ " " ++ URI++ " " ++ atom_to_list(Version)  ++ "\r\n" ++ "\r\n\r\n" ++ Body;

format_request_string(Method, URI, Version, Headers, Body)->
    string:to_upper(atom_to_list(Method)) ++ " " ++ URI ++ " " ++ atom_to_list(Version)  ++ "\r\n" ++ lists:flatten(Headers) ++ "\r\n" ++ Body.
