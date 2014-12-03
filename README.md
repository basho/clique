Riak CLI is a library for building command line interfaces in Erlang. It provides
command and option parsing, configuration showing and setting using cuttlefish,
and a small status type API that enables pretty printing.

# CLI usage
``riak_cli`` allows you define a command line interface using the Erlang API
described below. The interface provides both configuration and arbitrary
command handling. In the interest of clarity, a few examples will be given to
illustrate it's usage.

```shell

# Show the configuration for 2 config variables. Multiple values can be
# shown by using spaces between them. The --all flag means: give me the values
# on all nodes in the cluster.

$ dev/dev1/bin/riak-admin show transfer_limit leveldb.limited_developer_mem --all
+--------------+--------------+-----------------------------+
|     Node     |transfer_limit|leveldb.limited_developer_mem|
+--------------+--------------+-----------------------------+
|dev1@127.0.0.1|      4       |            true             |
|dev2@127.0.0.1|      6       |            true             |
+--------------+--------------+-----------------------------+


# Set the transfer_limit config on dev2
$ dev/dev1/bin/riak-admin set transfer_limit=6 --node=dev2@127.0.0.1
Set transfer limit for 'dev2@127.0.0.1' to 6

# Run an aribtrary, user defined command (this one doesn't exist... yet)
$ dev/dev1/bin/riak-admin handoff status

```

# Erlang API
The public API lives in
[riak_cli.erl](https://github.com/basho/riak_cli/blob/master/src/riak_cli.erl).
Users register functionality for given commands and configuration. When a
command is run, the code is appropriately dispatched so that the registered
actions are used. The goal is to minimize the user API, while making the overall
operation of the CLI more consistent.

### register_node_finder/1
Configuration can be set and shown across nodes. In order to contact the
appropriate nodes, the application needs to tell `riak_cli` how to determine that.
`riak_core` would do this in the following manner:

```erlang
F = fun() ->
        {ok, MyRing} = riak_core_ring_manager:get_my_ring(),
        riak_core_ring:all_members(MyRing)
    end,
riak_cli:register_node_finder(F).
```

### register_config/2
Showing and setting configuration is handled automatically via integration with
cuttlefish. The application environment variables can be set across nodes using
the installed cuttlefish schemas. In some instances however, a configuration
change requires doing something else to the cluster besides just setting
variables. For instance, when reducing the ``transfer_limit``, we want to
shutdown any extra handoff processes so we don't exceed the new limit.

Configuration specific behaviour can be managed by registering a callback to
fire when a given configuration variable is set on the cli. The callback runs
*after* the corresponding environment variables are set. The callback function
is a 3-arity function that gets called with the original key (as a list of
strings()), the untranslated value to set (as a string()) and the flags passed
on the command-line. The flags can be either '--all' to run on all nodes, or
--node N to run on node N. If no flags are given the command should be executed
on the local node (where the cli command was run) only.

```erlang
-spec set_transfer_limit(Key :: [string()], Val :: string(),
                         Flags :: [{atom(), proplist()}]).
...

Key = ["transfer_limit"],
Callback = fun set_transfer_limit/3,
riak_cli:register_config(Key, Callback).
```

#### A useful pattern
A good idea would be to centralize the command and usage registrations
into a single function for each module which implements CLI callbacks.

When starting an OTP-compliant application, make calls to these centralized
functions in the application's `start/2` call. (This is typically in the
`appname_app.erl` module.)

### register_command/4
Users can create their own CLI commands that are not directly configuration
related. These commands are relatively free-form, with the only restrictions
being that arguments are key/value pairs and flags come after arguments. For
example: `riak-admin transfer limit --node=dev2@127.0.0.1`. In this case the
command is "riak-admin transfer limit" which gets passed a `--node` flag. There
are no k/v arguments. These commands can be registered with riak_cli in the
following manner:

```erlang
%% Note that flags will be typecast using the typecast function and passed back
%% in the proplist as the converted type and not a string.
%% 
Cmd = ["riak-admin", "handoff", "limit"],
%% Keyspecs look identical to flagspecs but only have a typecast property.
%% There are no key/value arguments for this command
KeySpecs = [],
FlagSpecs = [{node, [{shortname, "n"},
                     {longname, "node"},
                     {typecast, fun list_to_atom/1}]}].
Fun = fun show_handoff_limit/2,
riak_cli:register_command(Cmd, KeySpecs, FlagSpecs, Fun).
```

#### Command callback implementation
The function which is registered as the callback for this command gets two
arguments.  One is a proplist of key/value pairs (if any, appropriately
typecast as specified), and the other is a proplist of flags (if any, also
appropriately typecast).  If the user provided the "shortname" of a flag, it is
provided as its character in the proplist.

For example if there was a command with a flag `--node` and a shortname of
`-n`, the proplist key would be `$n` for the shortname usage and the atom
`node` for the longname usage.

The return value of the callback function is thrown away unless it is tuple
`{error, Reason}`.

```erlang
cmd_callback([] = _KVs, [{$n, Node}]) ->
    cmd_callback([], [{node, Node}]);
cmd_callback([], [{node, Node}]) ->
    do_cmd_callback(Node);
cmd_callback([], []) ->
    %% no key/values or flags
    %% do default behavior
    do_cmd_callback(node()).

do_cmd_callback(Node) ->
    case frobulate(Node) of
        ok ->
            io:format("Success!~n", []);
        {error, Reason} ->
            {error, Reason};
        Other ->
            io:format("Weird! I got ~p~n", [Other])
    end.
```

### register_usage/2
After a few iterations on this design, we realized that having usage strings
embedded in command specs and autogenerating them wasn't the most
straightforward way to go. We want to show usage explicitly in many cases, and
not with the `--help` flag. Furthermore we want to allow some flexibility in the
output and enable long form documentation (properly formatted.) To make this
easier, the user must explicitly register usage points. If one of these points
is hit, the registered Usage string will be shown. Note that "Usage: " will be
prepended to the string, so don't add that part in.

```erlang
handoff_usage() ->
    ["riak-admin handoff <sub-command>\n\n",
     "  View handoff related status\n\n",
     "  Sub-commands:\n",
     "    limit      Show transfer limit\n\n"
    ].

handoff_limit_usage() ->
    ["riak-admin handoff limit [[--node | -n] <Node>] [--force-rpc | -f]\n\n",
     "  Show the handoff concurrency limits (transfer_limit) on all nodes.\n\n",
     "Options\n\n",
     "  -n <Node>, --node <Node>\n",
     "      Show the handoff limit for the given node only\n\n",
     "  -f, --force-rpc\n",
     "      Retrieve the latest value from a given node or nodes via rpc\n",
     "      instead of using cluster metadata which may not have propogated\n",
     "      to the local node yet. WARNING: The use of this flag is not\n",
     "      recommended as it spams the cluster with messages instead of\n",
     "      just talking to the local node.\n\n"
     ].

riak_cli:register_usage(["riak-admin", "handoff"], handoff_usage()),
riak_cli:register_usage(["riak-admin", "handoff", "limit"], handoff_limit_usage()).
```

### print/1
In some cases, user code will have a status formatted output generated using
riak_cli_status.erl. It may want to format this for display on the console. In
that case it would take the given status and call ``riak_cli:print(Status)``.

```erlang
%% The first element of the first row's elements is used as the titles for the
%% columns. All rows should have matching proplists forms.
Table = [[{node, "dev1@127.0.0.1"}, {num_connections, 100}],
          {node, "dev2@127.0.0.1"}, {num_connections, 90}]],

%% Note that a Status is a list of riak_cli_status formatted values.
Status = [riak_cli_status:table(Table)],
riak_cli:print(Status).
```

### run/1
This is the simplest command to use of all. It takes a given command as a list of
strings and attempts to run the command using the registered information. If
called with `set` or `show` as the first parameter the command is treated as
configuration and uses the registerd config callbacks and cuttlefish. Otherwise
the command is not configuration related and the other registered info is
used. This should only need to be called in one place in a given application. In
riak_core it gets called in ``riak_core_console:command/1``.

```erlang
%% New CLI API
-export([command/1]).

-spec command([string()]) -> ok.
command(Cmd) ->
    riak_cli_manager:run(Cmd).
```

# Status API
Riak CLI provides pretty printing support for status information. In order to do
this it requires status to be formatted in a specific manner when returned from
a command. All custom commands should return a type of `riak_cli_status:status()`.
An example is above in the documentation for `print/1`.

The entire status api lives [here](https://github.com/basho/riak_cli/blob/master/src/riak_cli_status.erl).

The api functions should be used instead of creating the status types directly.
The status types are output via the [riak_cli_writer](https://github.com/basho/riak_cli/blob/master/src/riak_cli_writer.erl).
