%%%-------------------------------------------------------------------
%%% @author Kim Hammar <kimham@kth.se>
%%% @copyright (C) 2016, Kim Hammar
%%% @doc
%%% Unit test suite for marley_router.erl
%%% @end
%%% Created :  2 Aug 2016 by Kim Hammar <kimham@kth.se>
%%%-------------------------------------------------------------------
-module(marley_router_tests).

%% API
-compile(export_all).

%% Includes
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% TESTS
%%%===================================================================
validate_routes_test_()->
    [
     ?_assertError(_, marley_router:validate_routes([])),
     ?_assertError(_, marley_router:validate_routes({})),
     ?_assertError(_, marley_router:validate_routes(ok)),
     ?_assertMatch(false, marley_router:validate_routes(#{})),
     ?_assertMatch(false, marley_router:validate_routes(#{router => [], static => []})),
     ?_assertMatch(false, marley_router:validate_routes(#{router => router, static => router})),
     ?_assertMatch(true, marley_router:validate_routes(#{router => router, static => []})),
     ?_assertMatch(true, marley_router:validate_routes(#{router => router, static => "path"})),
     ?_assertMatch(true, marley_router:validate_routes(#{static => "path"}))
    ].

route_test_()->
    {"Routing tests", {foreach,
                       fun()->
                               meck:new(client_router)
                       end,
                       fun(_) ->
                               meck:unload(client_router)
                       end,
                       [
                        fun() ->
                                Request = #{body => <<>>, headers => [], 
                                            request_line => #{http_method => get, 
                                                              http_uri => <<"/index">>,
                                                              http_version => <<"HTTP/1.0">>}},
                                Routes = #{router => client_router, static => "priv"},
                                meck:expect(client_router, get, fun(<<"/index">>,_,_) ->
                                                                        {200,<<"Hello World">>, <<>>} end),
                                ?assertMatch({200, <<"Hello World">>,<<>>}, marley_router:route(Request, Routes))
                        end,
                        fun() ->
                                Request = #{body => <<>>, headers => [], 
                                            request_line => #{http_method => get, 
                                                              http_uri => <<"/index">>,
                                                              http_version => <<"HTTP/1.0">>}},
                                Routes = #{router => client_router, static => "priv"},
                                meck:expect(client_router, get, fun(<<"/weird_route">>,_,_) ->
                                                                        {200,<<"Hello World">>,<<>>} end),
                                ?assertMatch({404,<<"/index not found">>,<<>>}, marley_router:route(Request, Routes))
                        end,
                        fun() ->
                                Request = #{body => <<>>, headers => [], 
                                            request_line => #{http_method => get, 
                                                              http_uri => <<"/index">>,
                                                              http_version => <<"HTTP/1.0">>}},
                                Routes = #{router => client_router, static => "priv"},
                                ?assertMatch({404,<<"/index not found">>,<<>>}, marley_router:route(Request, Routes))
                        end,
                        fun() ->
                                Request = #{body => <<>>, headers => [], 
                                            request_line => #{http_method => get, 
                                                              http_uri => <<"/index">>,
                                                              http_version => <<"HTTP/1.0">>}},
                                Routes = #{static => "priv"},
                                ?assertMatch({404,<<"/index not found">>,<<>>}, marley_router:route(Request, Routes))
                        end,
                        fun() ->
                                Request = #{body => <<>>, headers => [], 
                                            request_line => #{http_method => post, 
                                                              http_uri => <<"/index">>,
                                                              http_version => <<"HTTP/1.0">>}},
                                Routes = #{router => client_router, static => "priv"},
                                meck:expect(client_router, get, fun(<<"/weird_route">>,_, _) ->
                                                                        {200,<<"Hello World">>,<<>>} end),
                                ?assertMatch({400,<<>>,<<>>}, marley_router:route(Request, Routes))
                        end,
                        fun() ->
                                Request = #{body => <<>>, headers => [], 
                                            request_line => #{http_method => post, 
                                                              http_uri => <<"/index">>,
                                                              http_version => <<"HTTP/1.0">>}},
                                Routes = #{router => client_router, static => "priv"},
                                CreatedResource = #{<<"name">> => <<"marley">>, <<"duty">> => <<"serve the web">>},
                                meck:expect(client_router, post, fun(<<"/index">>,_,_) ->
                                                                         {201, CreatedResource, <<>>} end),
                                ?assertMatch({201,CreatedResource,<<>>}, marley_router:route(Request, Routes))
                        end,
                        fun() ->
                                Request = #{body => <<>>, headers => [], 
                                            request_line => #{http_method => get, 
                                                              http_uri => <<"/index">>,
                                                              http_version => <<"HTTP/1.0">>}},
                                Routes = #{router => client_router, static => "priv"},
                                meck:expect(client_router, not_found, fun(<<"/index">>,_,_) ->
                                                                              {404, <<"Custom not found function">>, <<>>} end),
                                ?assertMatch({404, <<"Custom not found function">>, <<>>}, marley_router:route(Request, Routes))
                        end,
                        fun() ->
                                meck:new(file, [passthrough, unstick]),
                                Request = #{body => <<>>, headers => [], 
                                            request_line => #{http_method => get, 
                                                              http_uri => <<"/index">>,
                                                              http_version => <<"HTTP/1.0">>}},
                                Routes = #{static => "priv"},
                                meck:expect(file, read_file, fun(_) ->
                                                                     {ok, <<"Static File Mock">>} end),
                                ?assertMatch({200, <<"Static File Mock">>, <<>>}, marley_router:route(Request, Routes)),
                                meck:unload(file)
                        end,
                        fun() ->
                                meck:new(file, [passthrough, unstick]),
                                Request = #{body => <<>>, headers => [], 
                                            request_line => #{http_method => get, 
                                                              http_uri => <<"/index">>,
                                                              http_version => <<"HTTP/1.0">>}},
                                Routes = #{router => client_router, static => "priv"},
                                meck:expect(file, read_file, fun(_) ->
                                                                     {ok, <<"Static File Mock">>} end),
                                meck:expect(client_router, get, fun(<<"/wrong_route">>,_,_) ->
                                                                        {200,<<"Hello World">>, <<>>} end),
                                ?assertMatch({200,<<"Static File Mock">>, <<>>}, marley_router:route(Request, Routes)),
                                meck:unload(file)
                        end
                       ]}}.





%%%===================================================================
%%% Internal functions
%%%===================================================================

