%%%-------------------------------------------------------------------
%%% @author Kim Hammar <kimham@kth.se>
%%% @copyright (C) 2016, Kim Hammar
%%% @doc
%%% Unit test suite for marley_http.erl
%%% @end
%%% Created : 30 Jul 2016 by Kim Hammar <kimham@kth.se>
%%%-------------------------------------------------------------------
-module(marley_http_tests).

%% API
-compile(export_all).

%% Includes
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% TESTS
%%%===================================================================
parse_request_test_()->
    [
     ?_assertError(_, marley_http:parse_request(ok)),
     ?_assertError(_, marley_http:parse_request({})),
     ?_assertError(_, marley_http:parse_request(<<"">>)),
     ?_assertError(_, marley_http:parse_request(<<"GET /index">>)),
     ?_assertError(_, marley_http:parse_request(<<"POST HTTP/1.0">>)),
     ?_assertError(_, marley_http:parse_request(<<"POST /index HTTP/1.0\r\n\r\n">>)),
     ?_assertError(_, marley_http:parse_request(<<"/index HTTP/1.0\r\n\r\n\r\n">>)),
     ?_assertError(_, marley_http:parse_request(<<"POST /index \r\n\r\n\r\n">>)),
     ?_assertMatch(#{body := <<>>,
                     headers := [],
                     request_line := #{http_method := post,http_uri := <<"/index">>,http_version := 'HTTP/1.0'}}, marley_http:parse_request(<<"POST /index HTTP/1.0\r\n\r\n\r\n">>))
    ].

http_response_test_()->
    [
     fun() ->
             Status = marley_http:status(200),
             Body = <<>>,
             Size = integer_to_binary(byte_size(Body)),
             Result = <<"HTTP/1.1 ",Status/bits,"\r\n","Connection: Keep-Alive\r\n","Content-Length:",Size/bits,"\r\n\r\n" >>,
             ?assertMatch(Result, marley_http:http_response('HTTP/1.1',200,<<>>,<<>>))
     end,
     fun() ->
             Status = marley_http:status(200),
             Body = <<"Hello World">>,
             Size = integer_to_binary(byte_size(Body)),
             Result = <<"HTTP/1.1 ",Status/bits,"\r\n","Connection: Keep-Alive\r\n","Content-Length:",Size/bits,"\r\n","Cache-control: no-cache\r\n\r\n",Body/bits>>,
             io:format("Result: ~p ~n", [Result]),
             ?assertMatch(Result, marley_http:http_response('HTTP/1.1',200,<<"Hello World">>,<<"Cache-control: no-cache\r\n">>))
     end,
     ?_assertError(_, marley_http:http_response("HTTP/1.1", 200, <<>>, <<>>))
    ].

%%%===================================================================
%%% Internal functions
%%%===================================================================
