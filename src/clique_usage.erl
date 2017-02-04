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

-module(clique_usage).

%% API
-export([
    init/0,
    find/1,
    register/2,
    print/1
]).

-export_type([usage/0]).

-ifdef(TEST).
%-compile(export_all).
-export([teardown/0]).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(usage_table, clique_usage).
-define(usage_prefix, "Usage: ").

-type err() :: {error, term()}.
-type usage_function() :: fun(() -> iolist()).
-type usage() :: iolist() | usage_function().

init() ->
    _ = ets:new(?usage_table, [public, named_table]),
    ok.

-ifdef(TEST).
-spec teardown() -> ok.
teardown() ->
    _ = ets:delete(?usage_table),
    ok.
-endif.

%% @doc Register usage for a given command sequence. Lookups are by longest
%% match.
-spec register([string()], usage()) -> true.
register(Cmd, Usage) ->
    ets:insert(?usage_table, {Cmd, Usage}).

-spec print(iolist()) -> ok.
print(Cmd = [Script, "describe" | _]) ->
    Usage = clique_error:format(Script, {error, describe_no_args}),
    clique:print(Usage, Cmd);
print(Cmd = [Script, "show" | _]) ->
    Usage = clique_error:format(Script, {error, show_no_args}),
    clique:print(Usage, Cmd);
print(Cmd = [Script, "set" | _]) ->
    Usage = clique_error:format(Script, {error, set_no_args}),
    clique:print(Usage, Cmd);
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
        [{Cmd, Fun}] when is_function(Fun) ->
            [?usage_prefix, Fun()];
        [{Cmd, Usage}] ->
            [?usage_prefix, Usage];
        [] ->
            Cmd2 = lists:reverse(tl(lists:reverse(Cmd))),
            find(Cmd2)
    end.

-ifdef(TEST).
find_different_types_test_() ->
    {setup,
        fun init/0,
        fun(_) -> teardown() end,
        ?_test(begin
            String = "clique foo [-f]\n",
            Fun = fun() -> String end,
            ?MODULE:register(["fun", "usage"], Fun),
            ?MODULE:register(["string", "usage"], String),
            ?assertEqual([?usage_prefix, String], find(["fun", "usage"])),
            ?assertEqual([?usage_prefix, String], find(["string", "usage"])),
            ?assertMatch({error, _}, find(["foo"]))
        end)}.
-endif.
