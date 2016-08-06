%%%-------------------------------------------------------------------
%%% @author Kim Hammar <kimham@kth.se>
%%% @copyright (C) 2016, Kim Hammar
%%% @doc
%%% Basic HTTP server
%%% @end
%%% Created :  1 Aug 2016 by Kim Hammar <kimham@kth.se>
%%%-------------------------------------------------------------------
-module(marley_server).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {socket, acceptors, open_connections, max_connections, routes}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server with given configurations
%%
%% @spec start_link(Opts) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Opts) ->
    lager:info("marley_server starting..."),
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server.
%% Starts acceptor processes and returns the intial state of the server.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init([Port, AcceptorPoolSize, MaxConnections, Routes]) ->
    lager:info("Initializing marley_server to listen on port: ~p", [Port]),
    lager:info("with ~p listeners", [AcceptorPoolSize]),
    process_flag(trap_exit, true),
    {ok, Socket} =
        gen_tcp:listen(Port,
                       [binary, {active, false}, {reuseaddr, true}]),
    Acceptors = ets:new(acceptors, [private, set]),
    StartAcceptor  = fun() ->
                             Pid =
                                 spawn_link(fun() ->
                                                    marley_acceptor:start(
                                                      Socket, Routes,
                                                      ?SERVER) end),
                             ets:insert(Acceptors, {Pid})
                     end,
    [ StartAcceptor() || _ <- lists:seq(1, AcceptorPoolSize)],
    {ok, #state{socket = Socket,
                acceptors = Acceptors,
                open_connections = 0,
                max_connections = MaxConnections,
                routes = Routes}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates a normal shutdown of the server
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {stop, normal, ok, State}
%% @end
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% handle client_connected cast.
%% Starts a new acceptor process and updates the server state.
%%
%% @spec handle_cast(client_connected, State) -> {noreply, State}
%%
%% @end
%%--------------------------------------------------------------------
handle_cast(client_connected, State) ->
    lager:debug("Client connected, open connections ~p",
                [State#state.open_connections, State#state.acceptors]),
    {noreply, client_connected(State)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle exit-trap from a acceptor process due to a normal shutdown.
%% Updates the server state.
%%
%% @spec handle_info(Info, State) -> {noreply, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, normal}, State) ->
    lager:debug("Client connection: ~p closed.", [Pid]),
    {noreply, acceptor_died(State, Pid)};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle exit-trap from a acceptor process due to a unexpected shutdown.
%% Updates the server state.
%%
%% @spec handle_info(Info, State) -> {noreply, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, State) ->
    lager:error("Client connection (pid ~p) unexpectedly "
                "crashed:~n~p~n", [Pid, Reason]),
    {noreply, acceptor_died(State, Pid)}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. Cleanup for stopping the server happens here.
%%
%% @spec terminate(Reason, State) -> ok
%% @end
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    lager:info("Marley terminating, reason: ~p, state: ~p", [Reason, State]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Helper function that contains logic for when a client connects.
%% Spawns a new acceptor process and inserts the process in the
%% ETS-table of the state and also updates the count of open connections.
%% Returns the new state.
%%
%% @spec code_change(State) -> NewState}
%% @end
%%--------------------------------------------------------------------
-spec client_connected(tuple()) -> tuple().
client_connected(State)->
    Pid = spawn_link(fun() ->
                             marley_acceptor:start(
                               State#state.socket,
                               State#state.routes, ?SERVER) end),
    ets:insert(State#state.acceptors, {Pid}),
    State#state{open_connections = State#state.open_connections + 1}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Helper function that contains logic for when a acceptor-process terminates.
%% Deletes the terminated acceptor-process from the ETS table and updates
%% the count of open connections.
%% Returns the new state.
%%
%% @spec code_change(State, Pid) -> NewState}
%% @end
%%--------------------------------------------------------------------
-spec acceptor_died(tuple(), pid()) -> tuple().
acceptor_died(State, Pid)->
    ets:delete(State#state.acceptors, Pid),
    State#state{open_connections = State#state.open_connections - 1}.
