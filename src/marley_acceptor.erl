%%%-------------------------------------------------------------------
%%% @author Kim Hammar <kimham@kth.se>
%%% @copyright (C) 2016, Kim Hammar
%%% @doc
%%% Module for http acceptor-process.
%%% The process listens on a given listen-socket for incoming connections.
%%% When a connection is established, the process handles the connection
%%% and then terminates. Trap-signals of the process exit is sent to
%%% the gen_server.
%%% @end
%%% Created :  1 Aug 2016 by Kim Hammar <kimham@kth.se>
%%%-------------------------------------------------------------------
-module(marley_acceptor).

%% API
-export([start/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Entry point of the acceptor process
%%
%% @spec start(Socket, Routes, Server) -> ok |
%%                                        no_return()
%% @end
%%--------------------------------------------------------------------
-spec start(gen_tcp:socket(), map(), atom()) -> atom() |
                                                no_return().
start(Socket, Routes, Server)->
    lager:info("acceptor process starting"),
    accept(Socket, Routes, Server).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Listens for incomming connections on a given TCP socket.
%% When the process accepts a connection it should notify the
%% server about it so that the sever can spawn a new acceptor process.
%%
%% @spec accept(Socket, Routes, Server) -> ok |
%%                                         no_return()
%% @end
%%--------------------------------------------------------------------
-spec accept(gen_tcp:socket(), marley_router:marley_routs(), atom()) ->
                    atom() |
                    no_return().
accept(Socket, Routes, Server)->
    case gen_tcp:accept(Socket) of
        {ok, Client} ->
            lager:info("client connected on acceptor process: ~p",[self()]),
            lager:debug("client connected on acceptor process: ~p",[self()]),
            gen_server:cast(Server, client_connected),
            keepalive_loop(Client, Routes);
        {error, Reason}->
            exit({error, Reason})
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Controls wether the process should keep the client connection
%% alive or close it.
%%
%% @spec keepalive_loop(Client, Routes) -> ok
%% @end
%%--------------------------------------------------------------------
-spec keepalive_loop(gen_tcp:socket(), marley_router:marley_routes()) -> atom().
keepalive_loop(Client, Routes)->
    case handle_request(Client, Routes) of
        keep_alive ->
            keepalive_loop(Client, Routes);
        close ->
            lager:info("client connection with acceptor process closed"),
            lager:debug("client connection with acceptor process ~p closed",
                        [self()]),
            ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles a incoming request on the client connection
%%
%% @spec handle_request(Client, Routes) -> keep_alive |
%%                                         close
%% @end
%%--------------------------------------------------------------------
-spec handle_request(gen_tcp:socket(), marley_router:marley_routes()) -> atom().
handle_request(Client, Routes)->
    case gen_tcp:recv(Client, 0, 5000) of %%Close socket if it's idle for 5 sec
        {ok, Data} ->
            Request = parse_request(Data),
            handle_response(Client, Request, Routes),
            keepalive_or_close(Request);
        {error, Reason} -> %% Expected error
            lager:debug("error while listening for data on socket"),
            lager:debug("reason: ~p acceptor: ~p",[Reason, self()]),
            gen_tcp:close(Client),
            close
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks wether the keep alive header is set in the connection.
%%
%% @spec keepalive_or_close(Request) -> close |
%%                                      keep_alive
%% @end
%%--------------------------------------------------------------------
-spec keepalive_or_close(marley_http:parsed_http_request()) -> atom().
keepalive_or_close(Request)->
    case lists:keyfind("Connection", 1,
                       maps:get(headers, Request)) of
        {"Connection", Value} ->
            case string:to_lower(Value) of
                "close" ->
                    close;
                _ ->
                    keep_alive
            end;
        _ ->
            keep_alive
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Parses the incoming data into a erlang term representing a HTTP
%% request.
%%
%% @spec parse_request(Data) -> ParsedRequest
%% @end
%%--------------------------------------------------------------------
-spec parse_request(binary()) -> map().
parse_request(Data)->
    marley_http:parse_request(Data).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles a HTTP response that is to be sent on the client connection.
%% Constructs the response and pass it along for sending to the client.
%%
%% @spec handle_response(Client, Request, Routes) -> ok |
%%                                                   {error, Reason}
%% @end
%%--------------------------------------------------------------------
-spec handle_response(gen_tcp:socket(), marley_http:parsed_http_request(),
                      marley_router:marley_routes()) -> atom()|
                                                        {error, atom()
                                                         | inet:posix()}.
handle_response(Client, Request, Routes)->
    Version = maps:get(http_version, maps:get(request_line, Request)),
    Response =
        construct_response(Version, marley_router:route(Request, Routes)),
    send_response(Client, Response).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Constructs a http response given a set of parameters.
%%
%% @spec construct_response(Version, {Code, Body, Headers}) ->
%%                                                 HttpResponse
%% @end
%%--------------------------------------------------------------------
-spec construct_response(marley_http:parsed_http_version(),
                         {integer(), binary(), binary()}) -> binary().
construct_response(Version, {Code, Body, Headers})->
    marley_http:http_response(Version, Code, Body, Headers).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sends a HTTP response to the client
%%
%% @spec send_response(Client, Response) -> ok |
%%                                          {error, Reason}
%% @end
%%--------------------------------------------------------------------
-spec send_response(gen_tcp:socket(), binary()) -> atom()|
                                                   {error, atom()
                                                    | inet:posix()}.
send_response(Client, Response)->
    gen_tcp:send(Client, Response).
