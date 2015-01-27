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
-module(clique_human_writer).

%% @doc This module provides callback functions to the status parsing code in
%% clique_status:parse/3. It specifically formats the output for a human at the
%% console and handles an opaque context passed back during parsing.

-behavior(clique_writer).

%% API
-export([write/1]).

-include("clique_status_types.hrl").

-record(context, {alert_set=false :: boolean(),
                  output="" :: iolist()}).

-spec write(status()) -> {iolist(), iolist()}.
write(Status) ->
    Ctx = clique_status:parse(Status, fun write_status/2, #context{}),
    {Ctx#context.output, []}.

%% @doc Write status information in console format.
-spec write_status(elem(), #context{}) -> #context{}.
write_status(alert, Ctx=#context{alert_set=false}) ->
    Ctx#context{alert_set=true};
write_status(alert, Ctx) ->
    %% TODO: Should we just return an error instead?
    throw({error, nested_alert, Ctx});
write_status(alert_done, Ctx) ->
    Ctx#context{alert_set=false};
write_status({list, Data}, Ctx=#context{output=Output}) ->
    Ctx#context{output=Output++write_list(Data)};
write_status({list, Title, Data}, Ctx=#context{output=Output}) ->
    Ctx#context{output=Output++write_list(Title, Data)};
write_status({text, Text}, Ctx=#context{output=Output}) ->
    Ctx#context{output=Output++Text++"\n"};
write_status({table, Rows}, Ctx=#context{output=Output}) ->
    Ctx#context{output=Output++write_table(Rows)};
write_status(done, Ctx) ->
    Ctx.

-spec write_table([{iolist(), iolist()}]) -> iolist().
write_table([]) ->
    "";
write_table(Rows0) ->
    Schema = [Name || {Name, _Val} <- hd(Rows0)],
    Rows = [[Val || {_Name, Val} <- Row] || Row <- Rows0],
    Table = clique_table:autosize_create_table(Schema, Rows),
    io_lib:format("~ts~n", [Table]).

%% @doc Write a list horizontally
write_list(Title, Items) when is_atom(Title) ->
    write_list(atom_to_list(Title), Items);
%% Assume all items are of same type
write_list(Title, Items) when is_atom(hd(Items)) ->
    Items2 = [atom_to_list(Item) || Item <- Items],
    write_list(Title, Items2);
write_list(Title, Items) ->
    %% Todo: add bold/color for Title when supported
    Title ++ ":" ++ write_list(Items) ++ "\n".

write_list(Items) ->
    lists:foldl(fun(Item, Acc) ->
                    Acc++" "++Item
                end, "", Items).
