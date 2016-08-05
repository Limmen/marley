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
%% @private
%% @doc
%% Matches a request to a route if it exists.
%% @end
%%--------------------------------------------------------------------

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

router_route(Router, Method, URI, Body, Headers) ->
    try apply(Router, Method, [URI, Headers, Body]) of
        Result ->
            Result
    catch
        _:_ ->
            no_match
    end.

no_match(Method, Static, URI, Router, Body, Headers)->
    case Method of
        get ->
            static_route(Static, URI, Router, Body, Headers);
        _ ->
            bad_request()
    end.

no_match(Method, Static, URI)->
    case Method of
        get ->
            static_route(Static, URI);
        _ ->
            bad_request()
    end.


static_route(Static, URI)->
    case file:read_file(Static ++ "/" ++ URI) of
        {error, _} ->
            not_found(URI);
        {ok, Bin} ->
            {200, Bin, <<>>}
    end.

static_route(Static, URI, Router, Body, Headers)->
    case file:read_file(Static ++ "/" ++ URI) of
        {error, _} ->
            not_found(Router, URI, Body, Headers);
        {ok, Bin} ->
            {200, Bin, <<>>}
    end.

not_found(URI)->
    {404, <<URI/bits, " not found">>, <<>>}.

not_found(Router, URI, Body, Headers)->
    try apply(Router, not_found, [URI, Body, Headers]) of
        Response ->
            Response
    catch
        _:_ ->
            not_found(URI)
    end.

bad_request()->
    {400, <<>>, <<>>}.
