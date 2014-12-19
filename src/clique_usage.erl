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
-module(clique_usage).

-define(usage_table, clique_usage).

-type err() :: {error, term()}.

%% API
-export([init/0,
         find/1,
         register/2,
         print/1,
         print/2]).

init() ->
    _ = ets:new(?usage_table, [public, named_table]),
    ok.

%% @doc Register usage for a given command sequence. Lookups are by longest
%% match.
-spec register([string()], iolist()) -> true.
register(Cmd, Usage0) ->
    Usage = ["Usage: ", Usage0],
    ets:insert(?usage_table, {Cmd, Usage}).

-spec print(iolist()) -> ok.
print(Cmd) ->
    print(Cmd, "").

-spec print(iolist(), iolist()) -> ok.
print(Cmd, Fallback) ->
    Usage = find_best_usage(find(Cmd), Fallback),
    io:format("~ts~n", [Usage]).

find_best_usage([], []) ->
    "Error: Usage information not found for the given command";
find_best_usage([], Fallback) ->
    Fallback;
find_best_usage(Usage, _Fallback) ->
    Usage.

-spec find(iolist()) -> iolist() | err().
find([]) ->
    [];
find(Cmd) ->
    case ets:lookup(?usage_table, Cmd) of
        [{Cmd, Usage}] ->
            Usage;
        [] ->
            Cmd2 = lists:reverse(tl(lists:reverse(Cmd))),
            find(Cmd2)
    end.
