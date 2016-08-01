# marley

## Description

Very basic http-server in erlang.

[![Build Status](https://travis-ci.org/Limmen/marley.svg?branch=master)](https://travis-ci.org/Limmen/marley)

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

# Start shell with application loaded and listen for code changes
$ ./rebar3 auto

# Run release
$ ./rebar3 run

```

## Copyright and license

see: [LICENSE](./LICENSE)

Copyright (c) 2016 Kim Hammar