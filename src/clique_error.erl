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
-export([format/2]).

-type status() :: clique_status:status().
-type err() :: {error, term()}.

-spec format(string(), err()) -> status().
format(Cmd, {error, show_no_args}) ->
    status(io_lib:format("Usage: ~ts show <variable> ... [[--node | -n] <node> | --all]", [Cmd]));
format(Cmd, {error, describe_no_args}) ->
    status(io_lib:format("Usage: ~ts describe <variable> ...", [Cmd]));
format(Cmd, {error, set_no_args}) ->
    status(io_lib:format("Usage: ~ts set <variable>=<value> ... [[--node | -n] <node> | --all]",
                         [Cmd]));
format(_Cmd, {error, {no_matching_spec, Cmd}}) ->
    case clique_usage:find(Cmd) of
        {error, _} ->
            status(io_lib:format("Invalid command '~ts'", [Cmd]));
        Usage ->
            status(io_lib:format("~ts", [Usage]))
    end;
format(_Cmd, {error, {invalid_flag, Str}}) ->
    status(io_lib:format("Invalid flag: ~p", [Str]));
format(_Cmd, {error, {invalid_action, Str}}) ->
    status(io_lib:format("Invalid action: ~p", [Str]));
format(_Cmd, {error, invalid_number_of_args}) ->
    status("Invalid number of arguments");
format(_Cmd, {error, {invalid_key, Str}}) ->
    status(io_lib:format("Invalid key: ~p", [Str]));
format(_Cmd, {error, {invalid_argument, Str}}) ->
    status(io_lib:format("Invalid argument: ~p", [Str]));
format(_Cmd, {error, {invalid_args, Args}}) ->
    Arglist = lists:map(fun({Key, Val}) -> io_lib:format("~ts=~ts ", [Key, Val]) end, Args),
    status(io_lib:format("Invalid arguments: ~ts", [Arglist]));
format(_Cmd, {error, {invalid_flags, Flags}}) ->
    status(io_lib:format("Invalid Flags: ~p", [Flags]));
format(_Cmd, {error, {invalid_flag_value, {Name, Val}}}) ->
    status(io_lib:format("Invalid value: ~p for flag: ~p", [Val, Name]));
format(_Cmd, {error, {invalid_flag_combination, Msg}}) ->
    status(io_lib:format("Error: ~ts", [Msg]));
format(_Cmd, {error, {invalid_value, Val}}) ->
    status(io_lib:format("Invalid value: ~p", [Val]));
format(_Cmd, {error, {too_many_equal_signs, Arg}}) ->
    status(io_lib:format("Too many equal signs in argument: ~p", [Arg]));
format(_Cmd, {error, {invalid_config_keys, Invalid}}) ->
    status(io_lib:format("Invalid config keys: ~ts", [Invalid]));
format(_Cmd, {error, {invalid_config, {error, [_H|_T]=Msgs}}}) ->
    %% Cuttlefish deeply nested errors (original cuttlefish)
    status(string:join(lists:map(fun({error, Msg}) -> Msg end,
                                 Msgs), "\n"));
format(_Cmd, {error, {invalid_config, {errorlist, Errors}}}) ->
    %% Cuttlefish deeply nested errors (new cuttlefish error scheme)
    status(string:join(lists:map(fun error_map/1, Errors), "\n"));
format(_Cmd, {error, {invalid_config, Msg}}) ->
    status(io_lib:format("Invalid configuration: ~p~n", [Msg]));
format(_Cmd, {error, {rpc_process_down, Node}}) ->
    status(io_lib:format("Target process could not be reached on node: ~p~n", [Node]));
format(_Cmd, {error, {config_not_settable, Keys}}) ->
    status(io_lib:format("The following config keys are not settable: ~p~n", [Keys]));
format(_Cmd, {error, {nodedown, Node}}) ->
    status(io_lib:format("Target node is down: ~p~n", [Node]));
format(_Cmd, {error, bad_node}) ->
    status("Invalid node name");
format(_Cmd, {error, {conversion, _}}=TypeError) ->
    %% Type-conversion error originating in cuttlefish
    status(cuttlefish_error:xlate(TypeError)).


-spec status(string()) -> status().
status(Str) ->
    [clique_status:alert([clique_status:text(Str)])].

%% Here we can override cuttlefish error messages to make them more
%% useful in an interactive context
-spec error_map(cuttlefish_error:error()) -> iolist().
error_map({error, {unknown_variable, Variable}}) ->
    io_lib:format("Unknown variable: ~ts", [Variable]);
error_map({error, ErrorTerm}) ->
    cuttlefish_error:xlate(ErrorTerm).
