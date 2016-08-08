%%%-------------------------------------------------------------------
%%% @author kim <kim@limmen>
%%% @copyright (C) 2016, kim
%%% @doc
%%% Router module
%%% @end
%%% Created :  7 Aug 2016 by kim <kim@limmen>
%%%-------------------------------------------------------------------
-module(marley_example_router).

%% API
-export([get/3, post/3, not_found/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Handles get requests
%% @spec get(URI, Body, Headers) -> {Code, Body, Headers}
%% @end
%%--------------------------------------------------------------------
get(<<"/">>,_,_)->
    {200, <<"Home page">>, <<"content-type: text/plain\r\n">>};

get(<<"/index">>,_,_)->
    {200, <<"Response to http get request for /index">>, <<"content-type: text/plain\r\n">>}.

%%--------------------------------------------------------------------
%% @doc
%% Handles post requests
%% @spec get(URI, Body, Headers) -> {Code, Body, Headers}
%% @end
%%--------------------------------------------------------------------
post(<<"/resource">>,_,_)->
    Resource = <<"resource">>,
    {201, Resource, <<"content-type:text/plain\r\n">>}.
%%--------------------------------------------------------------------
%% @doc
%% Not found route
%% @spec get(URI, Body, Headers) -> {Code, Body, Headers}
%% @end
%%--------------------------------------------------------------------
not_found(Route,_,_)->
    {404, <<Route/bits," not found">>, <<"content-type:text/plain\r\n">>}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
