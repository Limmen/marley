%%%-------------------------------------------------------------------
%%% @author Kim Hammar <kimham@kth.se>
%%% @copyright (C) 2016, Kim Hammar
%%% @doc
%%% Property tests for marley_router
%%% @end
%%% Created :  2 Aug 2016 by Kim Hammar <kimham@kth.se>
%%%-------------------------------------------------------------------
-module(prop_marley_router).

%% API
-compile(export_all).

%% Includes
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================

prop_validate_routes() ->
    ?FORALL({Static, NotFoundHandler, Routes}, {list(char()), marley_router:marley_route_handler(), list({marley_http:parsed_http_method(), list(char()), marley_router:marley_route_handler()})}, 
            validate_routes_property(#{routes => lists:map(fun({Method, Path, Handler}) -> #{http_method => Method,
                                                                                             path => Path,
                                                                                             handler => Handler}
                                                           end, Routes),
                                       static => Static,
                                       notfound => NotFoundHandler})).


%%%===================================================================
%%% Generators
%%%===================================================================


%%%===================================================================
%%% Properties
%%%===================================================================

validate_routes_property(Routes)->
    marley_router:validate_routes(Routes).

%%%===================================================================
%%% Helper functions
%%%===================================================================

