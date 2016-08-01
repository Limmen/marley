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
-export([start_http/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Function that should startup the marley web server with given
%% configurations.
%% @todo Implement this function
%% @end
%%--------------------------------------------------------------------
-spec start_http(integer(), integer(), map()) -> atom().
start_http(_Port, _Proccesses, _Routes) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
