# Introduction
Riak CLI is an opinionated framework for building command line interfaces in
Erlang. It provides users with an interface that gives them enough power to
build complex CLIs, but enough constraint to make them appear consistent.

### Why Riak CLI ?
When building a CLI for an Erlang application users frequently run into the following
problems:

  * Output is inconsistent across commands and often implemented differently for
    each command with little re-use.
  * Output is frequently hard to read for humans, hard to parse for machines, or
    both.
  * Adding a new command to the system often results in glue code and extra work
    rather than just writing a function that gathers information or performs a
    given user action.
  * Setting and showing configuration often only works on a single node.
  * Configuration changes with runtime side effects are often difficult to
    implement.

Riak CLI provides a standard way of implementing status, command, usage and
configuration functionality while minimizing the amount of code needed to be
written by users of the library.

Riak CLI provides the application developer with the following capabilities:
 * Implement callbacks that handle a given cli command such as `riak-admin handoff enable outbound`
 * Register usage points to show the correct usage in the command hierarchy when
   an incomplete command is run or the user issues the `--help` flag.
 * Set, show and describe [cuttlefish](https://github.com/basho/cuttlefish)
   configuration across one or all nodes: i.e.  `riak-admin set anti-entropy=on --all`
 * Return a standard status format that allows output of a variety of content
   types: human-readable, csv, html, etc... (Note that currently only
   human-readable output is implmented)

### Why Not Riak CLI ?
 * You aren't writing a CLI
 * You don't want or need to use cuttlefish for configuration
 * You only have a few command permutations and the dependency would be overkill
 * You already wrote your own cli tool
 * You are a masochist
 * You dislike your users

# CLI usage
Riak CLI provides a consistent and flexible interface to the end user of your
application. In the interest of clarity, a few examples will be given to
illustrate common usage.

```shell
# Show the configuration for 2 config variables. Multiple values can be
# shown by using spaces between them. The --all flag means: give me the values
# on all nodes in the cluster.

$ riak-admin show transfer_limit leveldb.limited_developer_mem --all
+--------------+--------------+-----------------------------+
|     Node     |transfer_limit|leveldb.limited_developer_mem|
+--------------+--------------+-----------------------------+
|dev1@127.0.0.1|      4       |            true             |
|dev2@127.0.0.1|      6       |            true             |
+--------------+--------------+-----------------------------+

# Set the transfer_limit config on dev2
$ riak-admin set transfer_limit=6 --node=dev2@127.0.0.1
Set transfer limit for 'dev2@127.0.0.1' to 6

# Describe 1 or more configuration variables
# Note that the descriptions are the doc comments in the cuttlefish schema
$ riak-admin describe transfer_limit storage_backend
transfer_limit:
  Number of concurrent node-to-node transfers allowed.

storage_backend:
  Specifies the storage engine used for Riak's key-value data
  and secondary indexes (if supported).

# Run an aribtrary, user defined command
$ riak-admin handoff enable outbound
Handoff setting successfully updated

# Show usage information when a command is incompletely specified
$ riak-admin handoff enable
Usage: riak-admin handoff <enable | disable> <inbound | outbound | both> [[--node | -n] <Node>] [--all]

  Enable or disable handoffs on the specified node(s).
  If handoffs are disabled in a direction, any currently
  running handoffs in that direction will be terminated.

Options
  -n <Node>, --node <Node>
     Modify the setting on the specified node (default: local node only)
  -a, --all
     Modify the setting on every node in the cluster

```

# Erlang API
Riak CLI handles all parsing, validation, and type conversion of input data in a
manner similar to getopt. Riak CLI also handles all formatting and output of
status. The user code registers specifications, usage documentation and
callbacks in order to plug into Riak CLI. When a command is run, the code is
appropriately dispatched via the registry. Each registered callback returns a
[status type](https://github.com/basho/riak_cli/blob/develop/src/riak_cli_status.erl)
that allows riak_cli to format the output in a standardized way.

### register/1
Register is a convenience function that gets called by an app with a list
of modules that implement the ``riak_cli_handler`` behaviour. This behaviour
implements a single callback: ``register_cli/0``. This callback is meant to wrap
the other registration functions so that each individual command or logical set
of commands can live in their own module and register themselves appropriately.

```erlang
%% Register the handler modules
-module(riak_core_cli_registry).

riak_cli:register([riak_core_cluster_status_handler]).
```

```erlang
-module(riak_core_cluster_status_handler]).
-export([register_cli/0]).

-behaviour(riak_cli_handler).

register_cli() ->
    riak_cli:register_config(...),
    riak_cli:register_command(...).
```

### register_node_finder/1
Configuration can be set and shown across nodes. In order to contact the
appropriate nodes, the application needs to tell ``riak_cli`` how to determine that.
``riak_core`` would do this in the following manner:

```erlang
F = fun() ->
        {ok, MyRing} = riak_core_ring_manager:get_my_ring(),
        riak_core_ring:all_members(MyRing)
    end,
riak_cli:register_node_finder(F).
```

Note that this function should only be called once per beam.

### register_config/2
Showing, setting and describing configuration variables is handled automatically
via integration with cuttlefish. The application environment variables can be
set across nodes using the installed cuttlefish schemas. In some instances
however, a configuration change requires doing something else to the cluster
besides just setting variables. For instance, when reducing the
``transfer_limit``, we want to shutdown any extra handoff processes so we don't
exceed the new limit.

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

### register_command/4
Users can create their own CLI commands that are not directly configuration
related. These commands are relatively free-form, with the only restrictions
being that arguments are key/value pairs and flags come after arguments. For
example: `riak-admin transfer limit --node=dev2@127.0.0.1`. In this case the
command is "riak-admin transfer limit" which gets passed a `--node` flag. There are no k/v
arguments. These commands can be registered with riak_cli in the following
manner:

```erlang
Cmd = ["riak-admin", "handoff", "limit"],

%% Keyspecs look identical to flagspecs but only have a typecast property.
%% There are no key/value arguments for this command
KeySpecs = [],
FlagSpecs = [{node, [{shortname, "n"},
                     {longname, "node"},
                     {typecast, fun riak_cli_typecast:to_node/1}]}].

%% The function which is registered as the callback for this command gets two
%% arguments. One is a proplist of key/value pairs (if any, appropriately
%% typecast as specified), and the other is a proplist of flags (if any, also
%% appropriately typecast). The flags proplist contains the given "longname"
%% converted to an atom as the proplist key.
%%
%% The expected return value of the callback function is `riak_cli_status:status()`.
%%
%% This pattern matching works here because we know we only allow one flag in
%% the flagspec, and the callback only ever fires with valid flags.
Callback = fun([]=_Keys, [{node, Node}]=Flags) ->
               case riak_cli_nodes:safe_rpc(Node, somemod, somefun, []) of
                   {error, _} ->
                       Text = riak_cli_status:text("Failed to Do Something"),
                       [riak_cli_status:alert([Text])];
                   {badrpc, _} ->
                       Text = riak_cli_status:text("Failed to Do Something"),
                       [riak_cli_status:alert([Text])];
                   Val ->
                       Text = io_lib:format("Some Thing was done. Value = ~p~n", [Val]),
                       [riak_cli_status:text(Text)]
               end
           end,

riak_cli:register_command(Cmd, KeySpecs, FlagSpecs, Callback).
```
#### Command callback implementation

### register_usage/2
We want to show usage explicitly in many cases, and not with the `--help` flag.
To make this easier, the user must explicitly register usage points. If one of
these points is hit, via longest match with the command string, the registered
usage string will be shown. Note that "Usage: " will be prepended to the string,
so don't add that part in.

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

### run/1
`run/1` takes a given command as a list of strings and attempts to run the
command using the registered information. If called with `set`, `show`, or
describe as the first parameter the command is treated as configuration. `run/1`
should only need to be called in one place in a given application. In riak_core
it gets called in ``riak_core_console:command/1``.

```erlang
%% New CLI API
-export([command/1]).

-spec command([string()]) -> ok.
command(Cmd) ->
    %% Example Cmd = ["riak-admin", "handoff"]
    %% This is the way arguments get passed in from a shell script using Nodetool.
    %% They are passed into an escript main/1 function in the same manner, but
    %% without the script name.
    riak_cli:run(Cmd).
```

# Status API
Riak CLI provides pretty printing support for status information. In order to do
this it requires status to be formatted in a specific manner when returned from
a command. All custom commands should return a type of ``riak_cli_status:status()``.

## Types
Types are abstract and should be generated by invoking the status API instead of assembled directly.

* `text` - A text value.
* `column` - A list of related values.
* `table` - A matrix of related values.
* `alert` - A description of an error.

Only `alert` values contain nested status types; e.g., a table does not contain cells which are `text` status types.

## Type assembly functions

See descriptions above for the arguments to each.

* ``riak_cli_status:text/1`` - Takes an `iolist`, returns a `text` object.
* `riak_cli_status:column/2` - Takes a title (`iolist`) and values (a list of `iolist`) intended to be displayed consecutively.
* `riak_cli_status:table/1` - Takes a list of proplists, each representing a row in the table. The keys in the first row represent column headers; each following row (proplist) must contain the same number of tagged tuples but the keys are ignored.
* `riak_cli_status:alert/1` - Takes a list of status types representing an error condition.
