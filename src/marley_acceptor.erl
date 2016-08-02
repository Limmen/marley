%%%-------------------------------------------------------------------
%%% @author Kim Hammar <kimham@kth.se>
%%% @copyright (C) 2016, Kim Hammar
%%% @doc
%%%
%%% @end
%%% Created :  1 Aug 2016 by Kim Hammar <kimham@kth.se>
%%%-------------------------------------------------------------------
-module(marley_acceptor).

%% API
-export([start_link/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Entry point of the acceptor process.
%% @todo Implement this function
%% @end
%%--------------------------------------------------------------------
start_link(Socket, Routes, Server)->
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
%% gen_server:cast(Server, accepted),
%% @todo Implement this function
%% @end
%%--------------------------------------------------------------------
accept(Socket, Routes, Server)->
    case gen_tcp:accept(Socket) of
        {ok, Client} ->
            gen_server:cast(Server, client_connected),
            keepalive_loop(Client, Routes);
        {error, Error}->
            ok %% Todo log error
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Controls wether the process should keep the client connection
%% alive or close it.
%% @end
%%--------------------------------------------------------------------
keepalive_loop(Client, Routes)->
    case handle_request(Client, Routes) of
        keep_alive ->
            keepalive_loop(Client, Routes);
        close ->
            ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles a incoming request on the client connection
%% @end
%%--------------------------------------------------------------------
handle_request(Client, Routes)->
    case gen_tcp:recv(Client, 0, 1000) of %%Close socket if it's idle for 1 sec
        {ok, Data} ->
            Request = parse_request(Data),
            handle_response(Client, Request, Routes),
            keepalive_or_close(Request);
        {error, Error} ->
            gen_tcp:close(Client), %% log Error
            close
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks wether the keep alive header is set in the connection.
%% @end
%%--------------------------------------------------------------------
keepalive_or_close(Request)->
    case string:to_lower(lists:keyfind("Connection", 1,
                                       maps:get(headers, Request))) of
        "close" ->
            close;
        _ ->
            keep_alive
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Parses the incoming data into a erlang term representing a HTTP
%% request.
%% @end
%%--------------------------------------------------------------------
parse_request(Data)->
    marley_http:parse_request(Data).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles a HTTP response that is to be sent on the client connection
%% @end
%%--------------------------------------------------------------------
handle_response(Client, Request, Routes)->
    Version = maps:get(http_version, maps:get(request_line, Request)),
    Response = construct_response(Version, marley_router:get_route(Request, Routes)),
    send_response(Client, Response).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Constructs a http response given a request and other options.
%% @end
%%--------------------------------------------------------------------
construct_response(Version, {handler, Code, Callback})->
    ok;

construct_response(Version, {reponse, Code, Response})->
    marley_http:http_response(Version, Code, Response),
    ok;

construct_response(Version, {static, Code, File})->
    ok;

construct_response(Version, no_response)->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sends a HTTP response to the client
%% @end
%%--------------------------------------------------------------------
send_response(Client, Response)->
    gen_tcp:send(Client, Response).
