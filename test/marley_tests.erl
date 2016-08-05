%%%-------------------------------------------------------------------
%%% @author Kim Hammar <kimham@kth.se>
%%% @copyright (C) 2016, Kim Hammar
%%% @doc
%%% Integration tests for the marley web server
%%% @end
%%% Created :  4 Aug 2016 by Kim Hammar <kimham@kth.se>
%%%-------------------------------------------------------------------
-module(marley_tests).

%% API
-compile(export_all).

%% Includes
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% TESTS
%%%===================================================================

marley_test_()->
    {foreach, fun setup/0, fun tear_down/1, 
     [
      ?_test(not_found()),
      ?_test(custom_not_found()),
      ?_test(client_router_get_request()),
      ?_test(client_router_post_request())
     ]}.
        


not_found() ->
    marley:start_http(3004,  #{static => "priv"}),
    timer:sleep(100),
    {ok, Result} = httpc:request("http://127.0.0.1:3004/index"),
    {{Version,Code,Status},Headers,Body} = Result,
    ?assertMatch("HTTP/1.1", Version),
    ?assertMatch(404, Code),
    ?assertMatch("Not Found", Status),
    ?assert(length(Headers) =:= 2),
    ?assert((lists:keyfind("connection", 1, Headers) /= false)),
    ?assert((lists:keyfind("content-length", 1, Headers) /= false)),
    {"content-length", Val} = lists:keyfind("content-length", 1, Headers),
    ?assert(list_to_integer(Val) =:= length(Body)),
    ?assertMatch("/index not found", Body),
    application:stop(marley).

custom_not_found()->
    marley:start_http(3004,  #{static => "priv", router => client_router}),
    meck:expect(client_router, not_found, fun(_, _, _) ->
                                                  {404, <<"Custom not found function">>, <<>>} end),
    timer:sleep(100),
    {ok, Result} = httpc:request("http://127.0.0.1:3004/index"),
    {{Version,Code,Status},Headers,Body} = Result,
    ?assertMatch("HTTP/1.1", Version),
    ?assertMatch(404, Code),
    ?assertMatch("Not Found", Status),
    ?assert(length(Headers) =:= 2),
    ?assert((lists:keyfind("connection", 1, Headers) /= false)),
    ?assert((lists:keyfind("content-length", 1, Headers) /= false)),
    {"content-length", Val} = lists:keyfind("content-length", 1, Headers),
    ?assert(list_to_integer(Val) =:= length(Body)),
    ?assertMatch("Custom not found function", Body),
    application:stop(marley).

client_router_get_request()->
    marley:start_http(3004,  #{static => "priv", router => client_router}),
    meck:expect(client_router, get, fun(<<"/index">>, _, _) ->
                                            {200, <<"INDEX PAGE">>, <<>>} end),
    timer:sleep(100),
    {ok, Result} = httpc:request("http://127.0.0.1:3004/index"),
    {{Version,Code,Status},Headers,Body} = Result,
    ?assertMatch("HTTP/1.1", Version),
    ?assertMatch(200, Code),
    ?assertMatch("OK", Status),
    ?assert(length(Headers) =:= 2),
    ?assert((lists:keyfind("connection", 1, Headers) /= false)),
    ?assert((lists:keyfind("content-length", 1, Headers) /= false)),
    {"content-length", Val} = lists:keyfind("content-length", 1, Headers),
    ?assert(list_to_integer(Val) =:= length(Body)),
    ?assertMatch("INDEX PAGE", Body),
    application:stop(marley).

client_router_post_request()->
    marley:start_http(3004,  #{static => "priv", router => client_router}),
    meck:expect(client_router, post, fun(<<"/index">>, _, Body) ->
                                             {400, <<"HANDLE POST REQUEST", Body/bits>>, <<>>} end),
    timer:sleep(100),
    {ok, Result} = httpc:request(post, {"http://127.0.0.1:3004/index", [], [], []}, [], []),
    {{Version,Code,Status},Headers,Body} = Result,
    ?assertMatch("HTTP/1.1", Version),
    ?assertMatch(400, Code),
    ?assertMatch("Bad Request", Status),
    ?assert(length(Headers) =:= 2),
    ?assert((lists:keyfind("connection", 1, Headers) /= false)),
    ?assert((lists:keyfind("content-length", 1, Headers) /= false)),
    {"content-length", Val} = lists:keyfind("content-length", 1, Headers),
    ?assert(list_to_integer(Val) =:= length(Body)),
    ?assertMatch("HANDLE POST REQUEST", Body),
    application:stop(marley).
%%%===================================================================
%%% Internal functions
%%%===================================================================

setup()->
    inets:start(),
    meck:new(client_router).

tear_down(_)->
    meck:unload(client_router).

