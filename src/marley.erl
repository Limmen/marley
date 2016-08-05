%%%-------------------------------------------------------------------
%%% @author Kim Hammar <kimham@kth.se>
%%% @copyright (C) 2016, Kim Hammar
%%% @doc
%%% This module contains the API that is the entry-point for the
%%% Marley web-server.
%%% @end
%%% Created : 31 Jul 2016 by Kim Hammar <kimham@kth.se>
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
    lager:info("Starting the Marley webserver and neccessary depedencies"),
    start(),
    supervisor:start_child(marley_sup,
                           [[Port, AcceptorPoolSize, MaxConnections, Routes]]),
    {ok, started}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Auxilliary function that starts necessary applications
%% @spec start() -> {ok, started}
%% @end
%%--------------------------------------------------------------------
start()->
    lager:start(),
    application:start(marley),
    {ok, started}.
