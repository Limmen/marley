%%%-------------------------------------------------------------------
%%% @author kim <kim@limmen>
%%% @copyright (C) 2016, kim
%%% @doc
%%% Module specifying the structure of route specification and
%%% other functions dealing with route information.
%%% @end
%%% Created :  1 Aug 2016 by kim <kim@limmen>
%%%-------------------------------------------------------------------
-module(marley_router).

%% API
-export([]).
-export_type([marley_routes/0]).

%% Types

-type marley_routes():: #{routes => marley_route(),
                          static => list()
                         }.

-type marley_route():: #{http_method => marley_http:parsed_http_method(),
                         path => list(),
                         handler => marley_route_handle()}.

-type marley_route_handle():: {handler, atom()}
                            | {response, any()}.
%%%===================================================================
%%% API
%%%===================================================================

%%%===================================================================
%%% Internal functions
%%%===================================================================
