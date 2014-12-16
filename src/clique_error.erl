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
-export([format/1]).

-type status() :: clique_status:status().
-type err() :: {error, term()}.

-spec format(err()) -> status().
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
        "Not a Key/Value argument of format: ~p=<Value>: ~n", [Arg]));
format({error, {too_many_equal_signs, Arg}}) ->
    status(io_lib:format("Too Many Equal Signs in Argument: ~p~n", [Arg]));
format({error, {invalid_config_keys, Invalid}}) ->
    status(io_lib:format("Invalid Config Keys: ~s~n", [Invalid]));
format({error, config_no_args}) ->
    status("Config Operations require one or more arguments");
format({error, {invalid_config, {error, [_H|_T]=Msgs}}}) ->
    %% Cuttlefish deeply nested errors
    status(string:join(lists:map(fun({error, Msg}) -> Msg end,
                                 Msgs), "\n"));
format({error, {invalid_config, Msg}}) ->
    status(io_lib:format("Invalid Configuration: ~p~n", [Msg]));
format({error, bad_node}) ->
    status("Invalid node name").

-spec status(string()) -> status().
status(Str) ->
    [clique_status:alert([clique_status:text(Str)])].
