%%%-------------------------------------------------------------------
%%% @author Kim Hammar <kimham@kth.se>
%%% @copyright (C) 2016, Kim Hammar
%%% @doc
%%% Module specifying the structure of route specification and
%%% other functions dealing with route information.
%%% @end
%%% Created :  1 Aug 2016 by Kim Hammar <kimham@kth.se>
%%%-------------------------------------------------------------------
-module(marley_router).

%% API
-export([route/2, validate_routes/1]).
-export_type([marley_routes/0]).

%% Types

-type marley_routes():: #{router => atom(), static => list(char())}
                      | #{static => list(char())}.

%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Matches a request to a route.
%%
%% @spec route(Request, Routes) -> {Code, Body, Headers}.
%% @end
%%--------------------------------------------------------------------
-spec route(marley_http:parsed_http_request(), marley_routes()) -> {integer(), binary(), binary()}.
route(Request, Routes)->
    URI = maps:get(http_uri, maps:get(request_line, Request)),
    Method = maps:get(http_method, maps:get(request_line, Request)),
    Headers = maps:get(headers, Request),
    Body = maps:get(body, Request),
    Static = maps:get(static, Routes),
    case maps:is_key(router, Routes) of
        true ->
            Router = maps:get(router, Routes),
            case router_route(Router, Method, URI, Body, Headers) of
                no_match ->
                    no_match(Method, Static, URI, Router, Body, Headers);
                Response ->
                    Response
            end;
        false ->
            no_match(Method, Static, URI)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Validates a set of routes according to the route specification.
%%
%% @spec route(Routes) -> true |
%%                        false
%% @end
%%--------------------------------------------------------------------
-spec validate_routes(marley_routes()) -> true |
                                          false.
validate_routes(Routes) ->
    case maps:size(Routes) =:= 2 of
        true ->
            is_atom(maps:get(router, Routes))
                andalso is_list(maps:get(static, Routes));
        false ->
            maps:size(Routes) =:= 1
                andalso is_list(maps:get(static, Routes))
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks  for a matching route in the client's route module .
%%
%% @spec router_route(Router, Method, URI, Body, Headers) -> {Code, Body, Headers} 
%%                                                           | no_match
%% @end
%%--------------------------------------------------------------------
-spec router_route(atom(), marley_http:parsed_http_method(), binary(), binary(), binary()) -> {integer(), binary(), binary()}
                                                                                                  | atom().
router_route(Router, Method, URI, Body, Headers) ->
    try apply(Router, Method, [URI, Headers, Body]) of
        Result ->
            Result
    catch
        _:_ ->
            no_match
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If it's a get_request it checks for a matching route in the client's directory of static files,
%% otherwise calls for bad_request() response.
%%
%% @spec no_match(Method, Static, URI, Router, Body, Headers) -> {Code, Body, Headers}
%% @end
%%--------------------------------------------------------------------
-spec no_match(marley_http:parsed_http_method(), atom(), binary(), atom(), binary(), binary()) -> {integer(), binary(), binary()}.
no_match(Method, Static, URI, Router, Body, Headers)->
    case Method of
        get ->
            static_route(Static, URI, Router, Body, Headers);
        _ ->
            bad_request()
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If it's a get_request it checks for a matching route in the client's directory of static files,
%% otherwise calls for bad_request() response.
%%
%% @spec no_match(Method, Static, URI) -> {Code, Body, Headers}
%% @end
%%--------------------------------------------------------------------
-spec no_match(marley_http:parsed_http_method(), atom(), binary()) -> {integer(), binary(), binary()}.
no_match(Method, Static, URI)->
    case Method of
        get ->
            static_route(Static, URI);
        _ ->
            bad_request()
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks  for a matching route in the client's directory of static files.
%%
%% @spec static_route(Static, URI, Router, Body, Headers) -> {Code, Body, Headers}
%% @end
%%--------------------------------------------------------------------
-spec static_route(atom(), binary(), atom(), binary(), binary()) -> {integer(), binary(), binary()}.
static_route(Static, URI, Router, Body, Headers)->
    case file:read_file(Static ++ "/" ++ URI) of
        {error, _} ->
            not_found(Router, URI, Body, Headers);
        {ok, Bin} ->
            {200, Bin, <<>>}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks  for a matching route in the client's directory of static files.
%%
%% @spec static_route(Static, URI) -> {Code, Body, Headers}
%% @end
%%--------------------------------------------------------------------
-spec static_route(atom(), binary()) -> {integer(), binary(), binary()}.
static_route(Static, URI)->
    case file:read_file(Static ++ "/" ++ URI) of
        {error, _} ->
            not_found(URI);
        {ok, Bin} ->
            {200, Bin, <<>>}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns a not_found response from the clients route-module
%%
%% @spec not_found(Router, URI, Body, Headers) -> {Code, Body, Headers}
%% @end
%%--------------------------------------------------------------------
-spec not_found(atom(), binary(), binary(), binary()) -> {integer(), binary(), binary()}.
not_found(Router, URI, Body, Headers)->
    try apply(Router, not_found, [URI, Body, Headers]) of
        Response ->
            Response
    catch
        _:_ ->
            not_found(URI)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns a generic 404 not found response
%%
%% @spec not_found(URI) -> {Code, Body, Headers}
%% @end
%%--------------------------------------------------------------------
-spec not_found(binary()) -> {integer(), binary(), binary()}.
not_found(URI)->
    {404, <<URI/bits, " not found">>, <<>>}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns a generic bad_request response
%%
%% @spec bad_request() -> {Code, Body, Headers}
%% @end
%%--------------------------------------------------------------------
-spec bad_request() -> {integer(), binary(), binary()}.
bad_request()->
    {400, <<>>, <<>>}.
