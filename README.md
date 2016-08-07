# marley

[![Build Status](https://travis-ci.org/Limmen/marley.svg?branch=master)](https://travis-ci.org/Limmen/marley)

## Description

Very basic http-server in erlang.

## How to

### Start the server as follows:

```erlang
    Routes = #{static => "priv", router => marley_example_router},
    Port = 3000,
    marley:start_http(Port, Routes).

```

Where static is the directory from where to server static files and router is the name of the module of your router, port is the portnumber that the server will listen on.

### Create your router or place static files in your static folder

Your router should export functions on the following form:
```erlang
    httpmethod(URI, Body, Headers) ->
                    {Code, ResponseBody, ResponseHeaders}.
```
URI, Body, headers, ResponseBody, ResponseHeaders should be in binary format and Code should be a integer (http status code).

Headers should be separated by "\r\n".

For instance:
```erlang
get(<<"/index">>,_,_)->
    {200, <<"Response to http get request for /index">>, <<"content-type: text/plain\r\n">>}.

post(<<"/resource">>,_,_)->
    Resource = <<"resource">>,
    {201, Resource, <<"content-type:text/plain\r\n">>}.
```

There is also a special function that your router can export to handle the case when the requested resource was not found:

```erlang
not_found(Route,_,_)->
    {404, <<Route/bits," not found">>, <<"content-type:text/plain\r\n">>}.
```
### Next open your browser at:

[http://localhost:3000/](http://localhost:3000/)

See examples in /examples

## Usage
```bash
# build
$ ./rebar3 compile

# remove temporary files
$ ./rebar3 clean

# run tests
$ ./rebar3 alias testall

# validate codebase, runs: tests, linters, static code analysis
$ ./rebar3 alias validate

# Generate documentation with edoc
$ ./rebar3 edoc

# Start shell with application loaded
$ ./rebar3 shell

# Start shell with configuration
$ rebar3 shell --config config_marley.config

# Start shell with application loaded and listen for code changes
$ ./rebar3 auto

# Run release
$ ./rebar3 run

```

## Copyright and license

see: [LICENSE](./LICENSE)

Copyright (c) 2016 Kim Hammar