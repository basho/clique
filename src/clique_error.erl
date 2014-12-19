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
-module(clique_error).

%% API
-export([format/1, extract_alert/1]).

-type status() :: clique_status:status().
-type err() :: {error, term()}.

-spec format(err()) -> status().
format({error, show_no_args}) ->
    status("Usage: show <variable> ...");
format({error, describe_no_args}) ->
    status("Usage: describe <variable> ...");
format({error, set_no_args}) ->
    status("Usage: set <variable>=<value>");
format({error, {no_matching_spec, Cmd}}) ->
    case clique_usage:find(Cmd) of
        {error, _} ->
            status("Invalid Command~n");
        Usage ->
            status(io_lib:format("~s", [Usage]))
    end;
format({error, {invalid_flag, Str}}) ->
    status(io_lib:format("Invalid Flag: ~p~n", [Str]));
format({error, {invalid_action, Str}}) ->
    status(io_lib:format("Invalid Action: ~p~n", [Str]));
format({error, invalid_number_of_args}) ->
    status("Invalid number of arguments~n");
format({error, {invalid_key, Str}}) ->
    status(io_lib:format("Invalid key: ~p~n", [Str]));
format({error, {invalid_argument, Str}}) ->
    status(io_lib:format("Invalid argument: ~p~n", [Str]));
format({error, {invalid_args, Args}}) ->
    Arglist = lists:map(fun({Key, Val}) -> io_lib:format("~ts=~ts ", [Key, Val]) end, Args),
    status(io_lib:format("Invalid arguments: ~s~n", [Arglist]));
format({error, {invalid_flags, Flags}}) ->
    status(io_lib:format("Invalid Flags: ~p~n", [Flags]));
format({error, {invalid_flag_value, {Name, Val}}}) ->
    status(io_lib:format("Invalid value: ~p for flag: ~p~n", [Val, Name]));
format({error, {invalid_flag_combination, Msg}}) ->
    status(io_lib:format("Error: ~s~n", [Msg]));
format({error, {invalid_value, Val}}) ->
    status(io_lib:format("Invalid value: ~p~n", [Val]));
format({error, {invalid_kv_arg, Arg}}) ->
    status(io_lib:format(
        "Must be in the format ~ts=<value> ~n", [Arg]));
format({error, {too_many_equal_signs, Arg}}) ->
    status(io_lib:format("Too many equal signs in argument: ~p~n", [Arg]));
format({error, {invalid_config_keys, Invalid}}) ->
    status(io_lib:format("Invalid config keys: ~s~n", [Invalid]));
format({error, {invalid_config, {error, [_H|_T]=Msgs}}}) ->
    %% Cuttlefish deeply nested errors
    status(string:join(lists:map(fun({error, Msg}) -> Msg end,
                                 Msgs), "\n"));
format({error, {invalid_config, Msg}}) ->
    status(io_lib:format("Invalid configuration: ~p~n", [Msg]));
format({error, {rpc_process_down, Node}}) ->
    status(io_lib:format("Target process could not be reached on node: ~p~n", [Node]));
format({error, {nodedown, Node}}) ->
    status(io_lib:format("Target node is down: ~p~n", [Node]));
format({error, bad_node}) ->
    status("Invalid node name").

-spec status(string()) -> status().
status(Str) ->
    [clique_status:alert([clique_status:text(Str)])].

%% Grab the first alert message for cli operator errors
-spec extract_alert(status()) -> string().
extract_alert([]) ->
    "";
extract_alert([{alert, [{text, Msg}]}|_T]) ->
    Msg;
extract_alert([_H|T]) ->
    extract_alert(T).
