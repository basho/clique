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
-module(clique_status).

%% API
-export([parse/3,
         text/1,
         list/1,
         list/2,
         table/1,
         alert/1,
         is_status/1,
         usage/0]).

-include("clique_status_types.hrl").

-export_type([status/0]).

-spec parse(status(), fun(), Acc0 :: term()) -> term().
parse([], Fun, Acc) ->
    Fun(done, Acc);
%% Alert is currently the only non-leaf element
parse([{alert, Elem} | T], Fun, Acc) ->
    Acc1 = Fun(alert, Acc),
    Acc2 = parse(Elem, Fun, Acc1),
    Acc3 = Fun(alert_done, Acc2),
    parse(T, Fun, Acc3);
%% Leaf elements
parse([Elem | T], Fun, Acc) ->
    Acc1 = Fun(Elem, Acc),
    parse(T, Fun, Acc1).

%% @doc Is the given value a status type?
-spec is_status(any()) -> boolean().
is_status(L) when is_list(L) ->
    is_status(hd(L));
is_status({text, _}) ->
    true;
is_status({list, _, _}) ->
    true;
is_status({table, _, _}) ->
    true;
is_status({alert, _}) ->
    true;
is_status(_) ->
    false.

-spec text(iolist()) -> text().
text(IoList) ->
    {text, IoList}.

-spec list([iolist()]) -> status_list().
list(Values) ->
    {list, Values}.

-spec list(iolist(), [iolist()]) -> status_list().
list(Title, Values) ->
    {list, Title, Values}.

%% @doc A table is constructed from a list of proplists. Each proplist
%% represents a row in the table. The keys in the first row represent
%% column headers; each following row (proplist) must contain the same
%% number of tagged tuples but the keys are ignored.
-spec table([[{atom() | string(), term()}]]) -> table().
table(Proplists) ->
    {table, Proplists}.

%% A list of elements
-spec alert([status_list() | table() | text()]) -> alert().
alert(List) ->
    {alert, List}.

%% @doc Using the usage construct, a clique run can indicate that
%% clique should display status for the current level.
usage() ->
    usage.
