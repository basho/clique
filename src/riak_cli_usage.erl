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
-module(riak_cli_usage).

-define(usage_table, riak_cli_usage).

-type err() :: {error, term()}.

%% API
-export([init/0,
         find/1,
         register/2,
         print/1]).

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
    Usage = case find(Cmd) of
                {error, Error} ->
                    Error;
                Usage2 ->
                    Usage2
            end,
    io:format("~s", [Usage]).

-spec find(iolist()) -> iolist() | err().
find([]) ->
    {error, "Error: Usage information not found for the given command\n\n"};
find(Cmd) ->
    case ets:lookup(?usage_table, Cmd) of
        [{Cmd, Usage}] ->
            Usage;
        [] ->
            Cmd2 = lists:reverse(tl(lists:reverse(Cmd))),
            find(Cmd2)
    end.

