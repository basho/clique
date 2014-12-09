-module(riak_cli).

%% API
-export([register/1,
         register_node_finder/1,
         register_command/4,
         register_config/2,
         register_usage/2,
         run/1,
         print/1]).

-type err() :: {error, term()}.

-spec register([module()]) -> ok.
register(Modules) ->
    _ = [M:register_cli() || M <- Modules],
    ok.

%% @doc RPC calls when using the --all flag need a list of nodes to contact.
%% However, using nodes() only provides currently connected nodes. We want to
%% also report an alert for nodes that are not currently available instead of just
%% ignoring them. This allows the caller to define how we find the list of
%% cluster member nodes.
-spec register_node_finder(fun()) -> true.
register_node_finder(Fun) ->
    riak_cli_nodes:register(Fun).

%% @doc Register configuration callbacks for a given config key
-spec register_config([string()], fun()) -> true.
register_config(Key, Callback) ->
    riak_cli_config:register(Key, Callback).

%% @doc Register a cli command (i.e.: "riak-admin handoff status")
-spec register_command([string()], list(), list(), fun()) -> true.
register_command(Cmd, Keys, Flags, Fun) ->
    riak_cli_command:register(Cmd, Keys, Flags, Fun).

%% @doc Register usage for a given command sequence. Lookups are by longest
%% match.
-spec register_usage([string()], iolist()) -> true.
register_usage(Cmd, Usage) ->
    riak_cli_usage:register(Cmd, Usage).

%% @doc Take a list of status types and generate console output
-spec print(err() | riak_cli_status:status()) -> ok.
print({error, _}=E) ->
    Alert = riak_cli_error:format(E),
    print(Alert);
print(Status) ->
    Output = riak_cli_writer:write(Status),
    io:format("~ts", [Output]),
    ok.

%% @doc Run a config operation or command
-spec run([string()]) -> ok | {error, term()}.
run([_Script, "set" | Args]) ->
    print(riak_cli_config:set(Args));
run([_Script, "show" | Args]) ->
    print(riak_cli_config:show(Args));
run([_Script, "describe" | Args]) ->
    print(riak_cli_config:describe(Args));
run(Cmd0) ->
    case is_help(Cmd0) of
        {ok, Cmd} ->
            riak_cli_usage:print(Cmd);
        _ ->
            M0 = riak_cli_command:match(Cmd0),
            M1 = riak_cli_parser:parse(M0),
            M2 = riak_cli_parser:validate(M1),
            print(riak_cli_command:run(M2))
    end.

%% @doc Help flags always comes at the end of the command
-spec is_help(iolist()) -> {ok, iolist()} | false.
is_help(Str) ->
    [H | T] = lists:reverse(Str),
    case H =:= "--help" orelse H =:= "-h" of
        true ->
            {ok, lists:reverse(T)};
        false ->
            false
    end.
