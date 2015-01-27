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

-module(clique_writer).

-define(writer_table, clique_writers).

-define(BUILTIN_WRITERS, [
                          {"human", clique_human_writer},
                          {"csv", clique_csv_writer}
                         ]).

-export([
         init/0,
         register/2,
         write/2
        ]).

%% @doc This module provides a central place to register different output writers
%% with clique (e.g. human-readable, CSV, etc.)
%% There are some built in, but we also allow applications to register their
%% own custom writers if they so choose.

-include("clique_status_types.hrl").

%% TODO factor err type out into single clique:err() type - DRY!
-type err() :: {error, term()}.

%% First element of the return value is for stdout, second is stderr
-callback write(status()) -> {iolist(), iolist()}.

-spec init() -> ok.
init() ->
    _ = ets:new(?writer_table, [public, named_table]),
    ets:insert(?writer_table, ?BUILTIN_WRITERS),
    %% We don't want to make mochiweb into a hard dependency, so only load
    %% the JSON writer if we have the mochijson2 module available:
    case code:which(mochijson2) of
        non_existing ->
            ok;
        _ ->
            ets:insert(?writer_table, {"json", clique_json_writer})
    end,
    ok.

-spec register(string(), module()) -> true.
register(Name, Module) ->
    ets:insert(?writer_table, {Name, Module}).

-spec write(err() | clique_status:status(), string()) -> {iolist(), iolist()}.
write(Status, Format) ->
    case ets:lookup(?writer_table, Format) of
        [{Format, Module}] ->
            Module:write(Status);
        [] ->
            Error = io_lib:format("Invalid format ~p! Defaulted to human-readable.~n", [Format]),
            {Stdout, Stderr} = clique_human_writer:write(Status),
            {Stdout, [Stderr, "\n", Error]}
    end.
