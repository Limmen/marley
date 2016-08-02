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
-export([get_route/2, validate_routes/1]).
-export_type([marley_routes/0, marley_route_handler/0, marley_route/0]).

%% Types

-type marley_routes():: #{routes => list(marley_route()),
                          static => list(char()),
                          notfound => marley_route_handler()
                         }.

-type marley_route():: #{http_method => marley_http:parsed_http_method(),
                         path => list(char()),
                         handler => marley_route_handler()}.

-type marley_route_handler():: {handler, integer(), atom()}
                             | {response, integer(), binary()}.
%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Matches a request to a route if it exists.
%% @end
%%--------------------------------------------------------------------
get_route(Request, Routes)->
    URI = maps:get(http_uri, maps:get(request_line, Request)),
    Method = maps:get(http_method, maps:get(request_line, Request)),
    case lists:filter(fun({M, Path, _}) -> 
                              Path =:= URI andalso M =:= Method
                      end, maps:get(routes, Routes)) of
        [] ->
            get_static_route(Method, URI, Routes);
        [Route] ->
            maps:get(handler, Route)
    end.

validate_routes(#{routes := Routes, static := Static, notfound := NotFound})
  when is_list(Routes) andalso is_list(Static) andalso is_tuple(NotFound) ->
    lists:all(fun(Route) -> validate_route(Route) end, Routes) 
        andalso validate_handler(NotFound).
    
%%%===================================================================
%%% Internal functions
%%%===================================================================

validate_route(#{http_method := M, path := P, handler := H}) when
      is_atom(M) andalso is_list(P) andalso is_tuple(H) ->
    validate_handler(H).

validate_handler({handler, Code, Module}) when is_integer(Code)
                                               andalso is_atom(Module) ->
    true;

validate_handler({response, Code, Response}) when is_integer(Code) andalso 
                                                  is_binary(Response) ->
    true.

get_static_route(get, URI, Routes)->
    {ok, Resources} = file:list_dir(maps:get(static, Routes)),
    case lists:member(URI, Resources) of
        true ->
            {static, 200, URI};
        false ->
            get_notfound_route(Routes)
    end;

get_static_route(_,_,_)->
    no_response.

get_notfound_route(Routes)->
    maps:get(notfound, Routes).
