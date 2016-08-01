%%%-------------------------------------------------------------------
%%% @author kim <kim@limmen>
%%% @copyright (C) 2016, kim
%%% @doc
%%% This module contains the API that is the entry-point for the
%%% Marley web-server.
%%% @end
%%% Created : 31 Jul 2016 by kim <kim@limmen>
%%%-------------------------------------------------------------------
-module(marley).

%% API
-export([start_http/2, start_http/4]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Function that should startup the marley web server with given
%% configurations.
%% @spec start_http(Port, Routes) -> {ok, started}
%% @end
%%--------------------------------------------------------------------
-spec start_http(integer(), marley_router:marley_routes()) -> tuple().
start_http(Port, Routes)->
    start_http(Port, 100, 10000, Routes).

%%--------------------------------------------------------------------
%% @doc
%% Function that should startup the marley web server with given
%% configurations.
%% @spec start_http(Port, AcceptorPoolSize, MaxConnections, Routes) ->
%%           {ok, started}
%% @end
%%--------------------------------------------------------------------
-spec start_http(integer(), integer(), integer(),
                 marley_router:marley_routes()) -> tuple().
start_http(Port, AcceptorPoolSize, MaxConnections,  Routes) ->
    marley_server:start_link([Port, AcceptorPoolSize, MaxConnections, Routes]),
    {ok, started}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
