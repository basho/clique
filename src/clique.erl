%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.  All Rights Reserved.
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
         register_usage/2,
         run/1,
         print/2]).

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

%% @doc Register a cli command (i.e.: "riak-admin handoff status")
-spec register_command([string()], list(), list(), fun()) -> true.
register_command(Cmd, Keys, Flags, Fun) ->
    clique_command:register(Cmd, Keys, Flags, Fun).

%% @doc Register usage for a given command sequence. Lookups are by longest
%% match.
-spec register_usage([string()], iolist()) -> true.
register_usage(Cmd, Usage) ->
    clique_usage:register(Cmd, Usage).

%% @doc Take a list of status types and generate console output
-spec print(err() | clique_status:status(), [string()]) -> ok.
print(usage, Cmd) ->
    clique_usage:print(Cmd);
print({error, _}=E, Cmd) ->
    Alert = clique_error:format(E),
    print(Alert, Cmd);
print(Status, _Cmd) ->
    Output = clique_human_writer:write(Status),
    io:format("~ts", [Output]),
    ok.

%% @doc Run a config operation or command
-spec run([string()]) -> ok | {error, term()}.
run([_Script, "set" | Args] = Cmd) ->
    print(clique_config:set(Args), Cmd);
run([_Script, "show" | Args] = Cmd) ->
    print(clique_config:show(Args), Cmd);
run([_Script, "describe" | Args] = Cmd) ->
    print(clique_config:describe(Args), Cmd);
run(Cmd0) ->
    case is_help(Cmd0) of
        {ok, Cmd} ->
            clique_usage:print(Cmd);
        _ ->
            M0 = clique_command:match(Cmd0),
            M1 = clique_parser:parse(M0),
            M2 = clique_parser:validate(M1),
            print(clique_command:run(M2), Cmd0)
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
