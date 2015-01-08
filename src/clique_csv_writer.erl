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
-module(clique_csv_writer).

%% @doc Implements a writer module for clique which outputs tabular data in CSV format.

-behavior(clique_writer).

-export([
         write/1,
         write_status/2
        ]).

-include("clique_status_types.hrl").

-spec write(status()) -> iolist().
write(Status) ->
    Output = clique_status:parse(Status, fun write_status/2, []),
    lists:reverse(Output).

%% @doc Write status information in csv format.
%%
%% Anything other than a table is discarded, since there's no good way to represent non-tabular
%% data in CSV. We want to be able to use the CSV output directly without needing to strip
%% off any extranious stuff.
-spec write_status(elem(), iolist()) -> iolist().
write_status({table, Rows}, Output) ->
    [write_table(Rows) | Output];
write_status(_, Output) ->
    Output.

write_table([]) ->
    "";
write_table(Rows0) ->
    Schema = [Name || {Name, _Val} <- hd(Rows0)],
    Header = write_header(Schema),
    Rows = write_rows([[Val || {_Name, Val} <- Row] || Row <- Rows0]),
    [Header, Rows].

write_header(Schema) ->
    HeaderStrs = [format_val(Name) || Name <- Schema],
    [string:join(HeaderStrs, ","), "\r\n"].

write_rows(Rows) ->
    [write_row(R) || R <- Rows].

write_row(Row) ->
    ValStrs = [format_val(V) || V <- Row],
    [string:join(ValStrs, ","), "\r\n"].

format_val(V) when is_atom(V) ->
    format_val(atom_to_list(V));
format_val(V) when is_integer(V) ->
    format_val(integer_to_list(V));
format_val(V) when is_binary(V) ->
    format_val(unicode:characters_to_list(V, utf8));
format_val(Str0) when is_list(Str0) ->
    Str = string:strip(Str0),
    %% If we have any line breaks, double quotes, or commas, we must surround the value with
    %% double quotes.
    IsEvilChar = fun(C) -> lists:member(C, [$\r, $\n, $", $,]) end,
    case lists:any(IsEvilChar, Str) of
        true ->
            [$", escape(Str), $"];
        false ->
            Str
    end.

escape(Str) ->
    %% According to RFC 4180, any double quote chars in quoted fields
    %% should be escaped with extra double quotes. e.g. "aaa","b""bb","ccc"
    SplitStr = string:tokens(Str, "\""),
    string:join(SplitStr, "\"\"").
