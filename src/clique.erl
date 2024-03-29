%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014-2017 Basho Technologies, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(clique).

%% API
-export([register/1,
    register_node_finder/1,
    register_command/4,
    register_config/2,
    register_formatter/2,
    register_writer/2,
    register_config_whitelist/1,
    register_usage/2,
    run/1,
    print/2]).

-ifdef(TEST).
-export([
    create_test_dir/0,
    delete_test_dir/1,
    ensure_stopped/0
]).
-include_lib("eunit/include/eunit.hrl").
-endif.

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
    clique_nodes:register(Fun).

%% @doc Register configuration callbacks for a given config key
-spec register_config([string()], fun()) -> true.
register_config(Key, Callback) ->
    clique_config:register(Key, Callback).

%% @doc Register a configuration formatter for a given config key
-spec register_formatter([string()], fun()) -> true.
register_formatter(Key, Callback) ->
    clique_config:register_formatter(Key, Callback).

%% @doc Register a module for writing output in a specific format
-spec register_writer(string(), module()) -> true.
register_writer(Name, Module) ->
    clique_writer:register(Name, Module).

%% @doc Register a list of configuration variables that are settable.
%% Clique disallows setting of all config variables by default. They must be in
%% whitelist to be settable.
-spec register_config_whitelist([string()]) -> ok | {error, {invalid_config_keys, [string()]}}.
register_config_whitelist(SettableKeys) ->
    clique_config:whitelist(SettableKeys).

%% @doc Register a cli command (e.g.: "riak-admin handoff status", or "riak-admin cluster join '*'")
-spec register_command(['*' | string()], '_' | list(), list(), fun()) -> ok | {error, atom()}.
register_command(Cmd, Keys, Flags, Fun) ->
    clique_command:register(Cmd, Keys, Flags, Fun).

%% @doc Register usage for a given command sequence. Lookups are by longest
%% match.
-spec register_usage([string()], clique_usage:usage()) -> true.
register_usage(Cmd, Usage) ->
    clique_usage:register(Cmd, Usage).

%% @doc Take a list of status types and generate console output
-spec print({error, term()}, term()) -> {error, 1};
        ({clique_status:status(), integer(), string()}, [string()]) -> ok | {error, integer()};
        (clique_status:status(), [string()]) -> ok.
print({error, _} = E, Cmd) ->
    print(E, Cmd, "human"),
    {error, 1};
print({Status, ExitCode, Format}, Cmd) ->
    print(Status, Cmd, Format),
    case ExitCode of
        0 -> ok;
        _ -> {error, ExitCode}
    end;
print(Status, Cmd) ->
    print(Status, Cmd, "human"),
    ok.

-spec print(usage | err() | clique_status:status(), [string()], string()) ->
    ok | {error, integer()}.
print(usage, Cmd, _Format) ->
    clique_usage:print(Cmd);
print({error, _} = E, Cmd, Format) ->
    Alert = clique_error:format(hd(Cmd), E),
    print(Alert, Cmd, Format);
print(Status, _Cmd, Format) ->
    {Stdout, Stderr} = clique_writer:write(Status, Format),
    %% This is kind of a hack, but I'm not aware of a better way to do this.
    %% When the RPC call is executed, it replaces the group_leader with that
    %% of the calling process, so that stdout is automatically redirected to
    %% the caller. However, stderr is not. To get the correct PID for stderr,
    %% we need to do an RPC back to the calling node and get it from them.
    CallingNode = node(group_leader()),
    RemoteStderr = rpc:call(CallingNode, erlang, whereis, [standard_error]),
    io:format("~ts", [Stdout]),
    io:format(RemoteStderr, "~ts", [Stderr]).

%% @doc Run a config operation or command
-spec run([string()]) -> ok | {error, integer()}.
run(Cmd) ->
    M0 = clique_command:match(Cmd),
    M1 = clique_parser:parse(M0),
    M2 = clique_parser:extract_global_flags(M1),
    M3 = clique_parser:validate(M2),
    print(clique_command:run(M3), Cmd).

-ifdef(TEST).

-spec create_test_dir() -> string() | no_return().
%% Exported: Creates a new, empty, uniquely-named directory for testing.
create_test_dir() ->
    string:strip(?cmd("mktemp -d /tmp/" ?MODULE_STRING ".XXXXXXX"), both, $\n).

-spec delete_test_dir(Dir :: string()) -> ok | no_return().
%% Exported: Deletes a test directory fully, whether it exists or not.
delete_test_dir(Dir) ->
    ?assertCmd("rm -rf " ++ Dir).

-spec ensure_stopped() -> ok.
%% Exported: Makes sure application processes are all stopped.
ensure_stopped() ->
    % Tests may start portions of the app, so work through whatever may be
    % running. The final clique_manager:teardown/0 should delete any leftover
    % ETS tables, so by the time we're done here the environment should be
    % entirely cleaned up.
    _ = application:stop(clique),
    lists:foreach(fun(Proc) ->
        case erlang:whereis(Proc) of
            Pid when erlang:is_pid(Pid) ->
                erlang:unlink(Pid),
                erlang:exit(Pid, shutdown);
            _ ->
                ok
        end
    end, [clique_sup, clique_manager]),
    clique_manager:teardown().

basic_cmd_test() ->
    start_manager(),
    Cmd = ["clique-test", "basic_cmd_test"],
    Callback = fun(CallbackCmd, [], []) ->
        ?assertEqual(Cmd, CallbackCmd),
        put(pass_basic_cmd_test, true),
        [] %% Need to return a valid status, but don't care what's in it
    end,
    ?assertEqual(ok, register_command(Cmd, [], [], Callback)),
    ?assertEqual(ok, run(Cmd)),
    ?assertEqual(true, get(pass_basic_cmd_test)).

cmd_error_status_test() ->
    start_manager(),
    Cmd = ["clique-test", "cmd_error_status_test"],
    Callback = fun(_, [], []) -> {exit_status, 123, []} end,
    ?assertEqual(ok, register_command(Cmd, [], [], Callback)),
    ?assertEqual({error, 123}, run(Cmd)).

start_manager() ->
    %% May already be started from a different test, which is fine, but we
    %% don't want it linked to the test process.
    case clique_manager:start_link() of
        {ok, Pid} ->
            erlang:unlink(Pid);
        _ ->
            ok
    end.

-endif.
