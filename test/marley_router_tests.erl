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
     ?_assertError(_, marley_router:validate_routes(#{})),
     ?_assertError(_, marley_router:validate_routes(ok)),
     ?_assertError(_, marley_router:validate_routes(#{routes => [], static => [], notfound => #{}})),
     ?_assertError(_, marley_router:validate_routes(#{routes => [], static => [], 
                                                      notfound => #{http_method => get,
                                                                    path => []}})),
     ?_assertError(_, marley_router:validate_routes(#{routes => [], static => [], 
                                                      notfound => #{http_method => get,
                                                                    path => [],
                                                                    handler => {}}})),
     ?_assertError(_, marley_router:validate_routes(#{routes => [], static => [], 
                                                      notfound => #{http_method => get,
                                                                    path => [],
                                                                    handler => {handler, [], []}}})),
     ?_assertError(_, marley_router:validate_routes(#{routes => [], static => [], 
                                                      notfound => #{http_method => get,
                                                                    path => [],
                                                                    handler => {handler, 200, <<"ok">>}}})),
     ?_assertError(_, marley_router:validate_routes(#{routes => [], static => [], 
                                                      notfound => #{http_method => get,
                                                                    path => [],
                                                                    handler => {response, [], []}}})),
     ?_assertError(_, marley_router:validate_routes(#{routes => [], static => [], 
                                                      notfound => #{http_method => get,
                                                                    path => [],
                                                                    handler => {response, 200, module}}})),
     ?_assertMatch(true, marley_router:validate_routes(#{routes => [], static => [], 
                                                         notfound => #{http_method => get,
                                                                       path => [],
                                                                       handler => {handler, 200, module}}})),
     ?_assertMatch(true, marley_router:validate_routes(#{routes => [], static => [], 
                                                         notfound => #{http_method => get,
                                                                       path => [],
                                                                       handler => {response, 200, <<"Body">>}}}))
    ].

%%%===================================================================
%%% Internal functions
%%%===================================================================
