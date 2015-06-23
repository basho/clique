# Introduction
Clique is an opinionated framework for building command line interfaces in
Erlang. It provides users with an interface that gives them enough power to
build complex CLIs, but enough constraint to make them appear consistent.

### Why Clique ?
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

Clique provides a standard way of implementing status, command, usage and
configuration functionality while minimizing the amount of code needed to be
written by users of the library.

Clique provides the application developer with the following capabilities:
 * Implement callbacks that handle a given cli command such as `riak-admin handoff enable outbound`
 * Register usage points to show the correct usage in the command hierarchy when
   an incomplete command is run or the user issues the `--help` flag.
 * Set, show and describe [cuttlefish](https://github.com/basho/cuttlefish)
   configuration across one or all nodes: i.e.  `riak-admin set anti-entropy=on --all`
 * Return a standard status format that allows output of a variety of content
   types: human-readable, csv, html, etc... (Note that currently only
   human-readable, CSV, and JSON output formats are implemented)

### Why Not Clique ?
 * You aren't writing a CLI
 * You don't want or need to use cuttlefish for configuration
 * You only have a few command permutations and the dependency would be overkill
 * You already wrote your own cli tool
 * You are a masochist
 * You dislike your users

# CLI usage
Clique provides a consistent and flexible interface to the end user of your
application. In the interest of clarity, a few examples will be given to
illustrate common usage.

```console
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
Clique handles all parsing, validation, and type conversion of input data in a
manner similar to getopt. Clique also handles all formatting and output of
status. The user code registers specifications, usage documentation and
callbacks in order to plug into Clique. When a command is run, the code is
appropriately dispatched via the registry. Each registered callback returns a
[status type](https://github.com/basho/clique/blob/develop/src/clique_status.erl)
that allows clique to format the output in a standardized way.

### Load Schemas
Clique requires applications to load their cuttlefish schemas prior to calling `register_config/1` or
`register_config_whitelist/1`. Below shows how `riak_core` loads schemas in a flexible manner
allowing for release or test usage.

```erlang
load_schema() ->
    case application:get_env(riak_core, schema_dirs) of
        {ok, Directories} ->
            ok = clique_config:load_schema(Directories);
        _ ->
            ok = clique_config:load_schema([code:lib_dir()])
    end.
```

### register/1
Register is a convenience function that gets called by an app with a list
of modules that implement the ``clique_handler`` behaviour. This behaviour
implements a single callback: ``register_cli/0``. This callback is meant to wrap
the other registration functions so that each individual command or logical set
of commands can live in their own module and register themselves appropriately.

```erlang
%% Register the handler modules
-module(riak_core_cli_registry).

clique:register([riak_core_cluster_status_handler]).
```

```erlang
-module(riak_core_cluster_status_handler]).
-export([register_cli/0]).

-behaviour(clique_handler).

register_cli() ->
    clique:register_config(...),
    clique:register_command(...).
```

### register_node_finder/1
Configuration can be set and shown across nodes. In order to contact the
appropriate nodes, the application needs to tell ``clique`` how to determine that.
``riak_core`` would do this in the following manner:

```erlang
F = fun() ->
        {ok, MyRing} = riak_core_ring_manager:get_my_ring(),
        riak_core_ring:all_members(MyRing)
    end,
clique:register_node_finder(F).
```

Note that this function only needs to be called once per beam. The callback
itself is stored in an ets table, and calling `clique:register_node_finder/1`
again will overwrite it with a new function.

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
clique:register_config(Key, Callback).
```

### register_config_formatter/2
By default, the clique "show" command displays the underlying config value, as stored in the
corresponding application env variable (the one exception being values of type "flag", which are
automatically displayed by clique as the user-facing flag value defined in the cuttlefish schema).
In many cases this is fine, but sometimes there may be translations defined in the cuttlefish schema
which make it desirable to show config values in a different format than the one used by the
underlying Erlang code.

To show a specific config value using a different format than the underlying raw application
config, you can register a config formatter against that value's config key:

```erlang
F = fun(Val) ->
        case Val of
            riak_kv_bitcask_backend -> bitcask;
            riak_kv_eleveldb_backend -> leveldb;
            riak_kv_memory_backend -> memory;
            riak_kv_multi_backend -> multi
        end
    end,
clique:register_config_formatter(["storage_backend"], F).
```

### register_config_whitelist/1
A lot of configuration variables are not intended to be set at runtime. In order to prevent the user
from changing them and anticipating the system to use the new values, we don't allow setting of any
variable by default. Each configuration variable that is settable must be added to a whitelist.

```erlang
%% Fail Fast if we pass in a value that is not the name of a configuration variable
ok = register_config_whitelist(["transfer_limit", "handoff.outbound", "handoff.inbound"]).
```

Note that in the future we hope to remove the need for this function by adding support for whitelist
annotations to cuttlefish variables instead.

### register_command/4
Users can create their own CLI commands that are not directly configuration
related. These commands are relatively free-form, with the only restrictions
being that arguments are key/value pairs and flags come after arguments. For
example: `riak-admin transfer limit --node=dev2@127.0.0.1`. In this case the
command is "riak-admin transfer limit" which gets passed a `--node` flag. There are no k/v
arguments. These commands can be registered with clique in the following
manner:

```erlang
Cmd = ["riak-admin", "handoff", "limit"],

%% Keyspecs look identical to flagspecs but only have a typecast property.
%% There are no key/value arguments for this command
KeySpecs = [],
FlagSpecs = [{node, [{shortname, "n"},
                     {longname, "node"},
                     {typecast, fun clique_typecast:to_node/1}]}].

%% The function which is registered as the callback for this command gets two
%% arguments. One is a proplist of key/value pairs (if any, appropriately
%% typecast as specified), and the other is a proplist of flags (if any, also
%% appropriately typecast). The flags proplist contains the given "longname"
%% converted to an atom as the proplist key.
%%
%% The expected return value of the callback function is `clique_status:status()`.
%%
%% This pattern matching works here because we know we only allow one flag in
%% the flagspec, and the callback only ever fires with valid flags.
Callback = fun(["riak-admin", "handoff", "limit"]=_Cmd, []=_Keys, [{node, Node}]=_Flags) ->
               case clique_nodes:safe_rpc(Node, somemod, somefun, []) of
                   {error, _} ->
                       Text = clique_status:text("Failed to Do Something"),
                       [clique_status:alert([Text])];
                   {badrpc, _} ->
                       Text = clique_status:text("Failed to Do Something"),
                       [clique_status:alert([Text])];
                   Val ->
                       Text = io_lib:format("Some Thing was done. Value = ~p~n", [Val]),
                       [clique_status:text(Text)]
               end
           end,

clique:register_command(Cmd, KeySpecs, FlagSpecs, Callback).
```

#### Command Wildcards
Users can also use the '*' atom any number of times at the end of a command spec
to indicate wildcard fields in the command. This is useful for simple commands that
always requires certain arguments in a clear concise order. For instance, the command
`riak-admin cluster join <node>` always requires a node name to be specified,
and it would be cumbersome and redundant if a user had to type
`riak-admin cluster join --node=<node>` instead. However, it is recommended that this
feature be used sparingly, and only in cases with a small number of arguments that are
always specified in a clear, obvious order. Too many free-form arguments can impair usability,
and can lead to situations where it's easy to forget the command format or to specify the
arguments in the wrong order.

When a command is run, it will try to match the exact command spec first, and then look for
progressively fuzzier matches using wildcards, working from the end of the command backward.
For example, if the user runs registers commands for `["my-cmd", "foo", "bar"]`,
`["my-cmd", "foo", '*']`, and `["my-cmd", '*', '*']`, then running "my-cmd foo bar" will always
match the first spec, "my-cmd foo blub" will match the second spec, and "my-cmd baz blub" will
match the final spec.

The existence of wildcards is the sole reason that the user-inputted command strings are passed
to the callback. If a command is registered without wildcards, the the same command will always
be passed to the callback function, and so that particular argument can be ignored (as it was
in the example above).

#### Wildcard Keyspecs
Specifying keyspecs for a command has the advantage of doing type conversions and automatically
recognizing if particular keys are valid or not. However, in some cases users may wish to allow
any and all key/value pairs through to the callback. To achieve this, an '_' atom can be used
as a keyspec, and all key=value pairs will be passed to the command callback in a list of type
`[{string(), string()}]`.

### register_usage/2
We want to show usage explicitly in many cases, and not with the `--help` flag.
To make this easier, the user must explicitly register usage points. If one of
these points is hit, via longest match with the command string, the registered
usage string will be shown. Note that "Usage: " will be prepended to the string,
so don't add that part in.

If you'd like to generate usage output dynamically, pass a 0-arity
function that returns an `iolist()` and it will be called when output
is generated.

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
     io_lib:format("      This node is: ~p~n", [node()]),
     "  -f, --force-rpc\n",
     "      Retrieve the latest value from a given node or nodes via rpc\n",
     "      instead of using cluster metadata which may not have propagated\n",
     "      to the local node yet. WARNING: The use of this flag is not\n",
     "      recommended as it spams the cluster with messages instead of\n",
     "      just talking to the local node.\n\n"
     ].

%% Use a static iolist():
clique:register_usage(["riak-admin", "handoff"], handoff_usage()),

%% Register a callback for dynamic output:
clique:register_usage(["riak-admin", "handoff", "limit"], fun handoff_limit_usage/0).
```

### register_writer/2
This is not something most applications will likely need to use, but the
capability exists to create custom output writer modules. Currently you can
specify the `--format=[human|csv|json]` flag on many commands to determine how
the output will be written; registering a new writer "foo" allows you to use
`--format=foo` to write the output using whatever corresponding writer module
you've registered.

(Note that the JSON writer is a special case, in that it is only available if
the mochijson2 module is present at startup. We wanted to avoid having to
introduce MochiWeb as a hard dependency, so instead we allow users of Clique to
decide for themselves if/how they want to include the mochijson2 module.)

Writing custom output writers is relatively undocumented right now, and the
values passed to the `write/1` callback may be subject to future changes. But,
the `clique_*_writer` modules in the Clique source tree provide good examples
that can be used for reference.

### run/1
`run/1` takes a given command as a list of strings and attempts to run the
command using the registered information. If called with `set`, `show`, or
`describe` as the second argument in the list, the command is treated as
configuration. Note that the first argument is the program/script name. `run/1`
should only need to be called in one place in a given application. In riak_core
it gets called in ``riak_core_console:command/1`` via an rpc call from Nodetool
in the `riak-admin` shell script. The list of arguments given to run are the
actual arguments given in the shell and provided by Nodetool as a list of
strings. This format is the same format in which command line arguments get
passed to an escript `main/1` function. The difference is that when using
Nodetool you typically also pass the name of the script as the first argument,
while escripts only pass the paramaters not including the script name (argv0).

```erlang
%% New CLI API
-export([command/1]).

-spec command([string()]) -> ok.
command(Cmd) ->
    %% Example Cmd = ["riak-admin", "handoff"]
    %% This is the way arguments get passed in from a shell script using Nodetool.
    %% They are passed into an escript main/1 function in the same manner, but
    %% without the script name.
    clique:run(Cmd).
```

# Status API
Clique provides pretty printing support for status information. In order to do
this it requires status to be formatted in a specific manner when returned from
a command. All custom commands should return a type of ``clique_status:status()``.

## Types
Types are abstract and should be generated by invoking the status API instead of assembled directly.

* `text` - A text value.
* `list` - A list of related values, with or without a label
* `table` - A matrix of related values.
* `alert` - A description of an error.

Only `alert` values contain nested status types; e.g., a table does not contain cells which are `text` status types.

## Type assembly functions

See descriptions above for the arguments to each.

* ``clique_status:text/1`` - Takes an `iolist`, returns a `text` object.
* `clique_status:list/2` - Takes a title (`iolist`) and values (a list of `iolist`) intended to be displayed consecutively.
* `clique_status:list/1` - Takes a title-less list of values (as a list of `iolist`) intened to be displayed consecutively
* `clique_status:table/1` - Takes a list of proplists, each representing a row in the table. The keys in the first row represent column headers; each following row (proplist) must contain the same number of tagged tuples in the same order, and the keys are ignored.
* `clique_status:alert/1` - Takes a list of status types representing an error condition.
