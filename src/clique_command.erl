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
-module(clique_command).
-include("clique_specs.hrl").

-define(cmd_table, clique_commands).


%% API
-export([init/0,
         run/1,
         match/1,
         register/4]).

-type err() :: {error, term()}.
-type proplist() :: [{atom(), term()}].
-type status() :: clique_status:status().

-define(SET_CMD_SPEC, {["_", "set"], '_', clique_config:config_flags(), fun clique_config:set/2}).

init() ->
    _ = ets:new(?cmd_table, [public, named_table]),
    ok.

%% @doc Register a cli command (i.e.: "riak-admin handoff status")
-spec register([string()], list(), list(), fun()) -> true.
register(Cmd, Keys0, Flags0, Fun) ->
    Keys = make_specs(Keys0),
    Flags = make_specs(Flags0),
    ets:insert(?cmd_table, {Cmd, Keys, Flags, Fun}).

-spec run(err()) -> err();
         ({fun(), proplist(), proplist()})-> status().
run({error, _}=E) ->
    E;
run({Fun, Args, Flags, GlobalFlags}) ->
    Format = proplists:get_value(format, GlobalFlags, "human"),
    case proplists:is_defined(help, GlobalFlags) of
        true ->
            {usage, Format};
        false ->
            Result = Fun(Args, Flags),
            {Result, Format}
    end.

-spec match([list()])-> {tuple(), list()} | {error, no_matching_spec}.
match(Cmd0) ->
    {Cmd, Args} = split_command(Cmd0),
    %% Check for builtin commands first. If that fails, check our command table.
    case Cmd of
        [_Script, "set" | _] ->
            {?SET_CMD_SPEC, Args};
        [_Script, "show" | _] ->
            Spec = cmd_spec(Cmd, fun clique_config:show/2, clique_config:config_flags()),
            {Spec, Args};
        [_Script, "describe" | _] ->
            Spec = cmd_spec(Cmd, fun clique_config:describe/2, []),
            {Spec, Args};
        _ ->
            case ets:lookup(?cmd_table, Cmd) of
                [Spec] ->
                    {Spec, Args};
                [] ->
                    {error, {no_matching_spec, Cmd0}}
            end
    end.

-spec split_command([list()]) -> {list(), list()}.
split_command(Cmd0) ->
    lists:splitwith(fun(Str) ->
                        clique_parser:is_not_kv_arg(Str) andalso
                        clique_parser:is_not_flag(Str)
                    end, Cmd0).


-spec make_specs([{atom(), proplist()}]) -> [spec()].
make_specs(Specs) ->
    [ clique_spec:make(Spec) || Spec <- Specs ].

%% NB This is a bit sneaky. We normally only accept key/value args like
%% "handoff.inbound=off" and flag-style arguments like "--node dev1@127.0.0.1" or "--all",
%% but the builtin "show" and "describe" commands work a bit differently.
%% To handle these special cases, we dynamically build a command spec to smuggle the
%% arguments through the rest of the (otherwise cleanly designed and implemented) code.
cmd_spec(Cmd, CmdFun, AllowedFlags) ->
    [_Script, _CmdName | CfgKeys] = Cmd,
    %% Discard key/val args passed in since we don't need them, and inject the freeform args:
    SpecFun = fun([], Flags) -> CmdFun(CfgKeys, Flags) end,
    {Cmd, [], AllowedFlags, SpecFun}.
