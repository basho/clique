-module(riak_cli).

%% API
%% Just re-exported functions from riak_cli_manager, mewstly.
-export([register_command/5,
         register_config/2,
         register_usage/2,
         run/1,
         print/1]).

%% @doc Register configuration callbacks for a given config key
-spec register_config([string()], fun()) -> true.
register_config(Key, Callback) ->
    riak_cli_manager:register_config(Key, Callback).

%% @doc Register a cli command (i.e.: "riak-admin handoff status")
-spec register_command([string()], string(), list(), list(), fun()) -> true.
register_command(Cmd, Description, Keys, Flags, Fun) ->
    riak_cli_manager:register_command(Cmd, Description, Keys, Flags, Fun).

%% @doc Register usage for a given command sequence. Lookups are by longest
%% match.
-spec register_usage([string()], iolist()) -> true.
register_usage(Cmd, Usage) ->
    riak_cli_manager:register_usage(Cmd, Usage).

%% @doc Take a status type and generate console output
-spec print(riak_cli_status:status()) -> ok.
print(Status) ->
    riak_cli_manager:write_status(Status).

%% @doc Run a config operation or command
-spec run([string()]) -> ok | {error, term()}.
run(Strings) ->
    riak_cli_manager:run(Strings).
