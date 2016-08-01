%%%-------------------------------------------------------------------
%%% @author kim <kim@limmen>
%%% @copyright (C) 2016, kim
%%% @doc
%%% Unit test suite for marley_http.erl
%%% @end
%%% Created : 30 Jul 2016 by kim <kim@limmen>
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

%%%===================================================================
%%% Internal functions
%%%===================================================================
