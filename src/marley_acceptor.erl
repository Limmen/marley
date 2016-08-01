%%%-------------------------------------------------------------------
%%% @author kim <kim@limmen>
%%% @copyright (C) 2016, kim
%%% @doc
%%%
%%% @end
%%% Created :  1 Aug 2016 by kim <kim@limmen>
%%%-------------------------------------------------------------------
-module(marley_acceptor).

%% API
-export([start_link/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Entry point of the acceptor process.
%% @todo Implement this function
%% @end
%%--------------------------------------------------------------------
start_link(Socket, Routes)->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Listens for incomming connections on a given TCP socket.
%% When the process accepts a connection it should notify the
%% server about it so that the sever can spawn a new acceptor process.
%% @todo Implement this function
%% @end
%%--------------------------------------------------------------------
accept(_Opts)->
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Controls wether the process should keep the client connection
%% alive or close it.
%% @end
%%--------------------------------------------------------------------
keepalive_loop(_Opts)->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles a incoming request on the client connection
%% @end
%%--------------------------------------------------------------------
handle_request(_Opts)->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Parses the incoming data into a erlang term representing a HTTP
%% request.
%% @end
%%--------------------------------------------------------------------
parse_request(_Opts)->
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles a HTTP response that is to be sent on the client connection
%% @end
%%--------------------------------------------------------------------
handle_response(_Opts)->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Constructs a http response given a request and other options.
%% @end
%%--------------------------------------------------------------------
construct_response(_Opts)->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sends a HTTP response to the client
%% @end
%%--------------------------------------------------------------------
send_response(_Opts)->
    ok.
