-module(riak_cli).

%% API
%% Just re-exported functions from riak_cli_manager, mewstly.
-export([register_node_finder/1,
         register_command/4,
         register_config/2,
         register_usage/2,
         run/1,
         print/1]).

%% @doc RPC calls when using the --all flag need a list of nodes to contact.
%% However, using nodes() only provides currently connected nodes. We want to
%% also report an alert for nodes that are not currently available instead of just
%% ignoring them. This allows the caller to define how we find the list of
%% cluster member nodes.
-spec register_node_finder(fun()) -> [node()].
register_node_finder(Fun) ->
    riak_cli_manager:register_node_finder(Fun).

%% @doc Register configuration callbacks for a given config key
-spec register_config([string()], fun()) -> true.
register_config(Key, Callback) ->
    riak_cli_manager:register_config(Key, Callback).

%% @doc Register a cli command (i.e.: "riak-admin handoff status")
-spec register_command([string()], list(), list(), fun()) -> true.
register_command(Cmd, Keys, Flags, Fun) ->
    riak_cli_manager:register_command(Cmd, Keys, Flags, Fun).

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
