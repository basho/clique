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

-define(cmd_table, clique_commands).


%% API
-export([init/0,
         run/1,
         match/1,
         register/4]).

-type err() :: {error, term()}.
-type proplist() :: [{atom(), term()}].
-type status() :: clique_status:status().

init() ->
    _ = ets:new(?cmd_table, [public, named_table]),
    ok.

%% @doc Register a cli command (i.e.: "riak-admin handoff status")
-spec register([string()], list(), list(), fun()) -> true.
register(Cmd, Keys, Flags, Fun) ->
    ets:insert(?cmd_table, {Cmd, Keys, Flags, Fun}).

-spec run(err()) -> err();
         ({fun(), proplist(), proplist()})-> status().
run({error, _}=E) ->
    E;
run({Fun, Args, Flags}) ->
    Fun(Args, Flags).

-spec match([list()])-> {tuple(), list()} | {error, no_matching_spec}.
match(Cmd0) ->
    {Cmd, Args} = split_command(Cmd0),
    case ets:lookup(?cmd_table, Cmd) of
        [Spec] ->
            {Spec, Args};
        [] ->
            {error, {no_matching_spec, Cmd0}}
    end.

-spec split_command([list()]) -> {list(), list()}.
split_command(Cmd0) ->
    lists:splitwith(fun(Str) ->
                        clique_parser:is_not_kv_arg(Str) andalso
                        clique_parser:is_not_flag(Str)
                    end, Cmd0).

