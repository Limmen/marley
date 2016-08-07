%%%-------------------------------------------------------------------
%% @doc marley_example public API
%% @end
%%%-------------------------------------------------------------------

-module(marley_example_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Routes = #{static => "priv", router => marley_example_router},
    Port = 3000,
    marley:start_http(Port, Routes),
    marley_example_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
