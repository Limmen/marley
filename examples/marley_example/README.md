# marley_example

## Description

Hello-world example of using the marley webserver

## Usage
```bash
# build
$ rebar3 compile

# Start shell with application loaded
$ ./rebar3 shell

# When shell is started, start dependencies:
$ lager:start().
$ application:start(marley).

# Start the example:
$ application:start(marley_example).

```

Open your browser at:

[http://localhost:3000/](http://localhost:3000/)