-module(riak_cli_error).

%% API
-export([format/1]).

-type status() :: riak_cli_status:status().
-type err() :: {error, term()}.

-spec format(err()) -> status().
format({error, {no_matching_spec, Cmd}}) ->
    case riak_cli_usage:find(Cmd) of
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
    status(io_lib:format("Invalid number of arguments~n"));
format({error, {invalid_argument, Str}}) ->
    status(io_lib:format("Invalid argument: ~p~n", [Str]));
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
    status("Config Operations require one or more arguments.");
format({error, {invalid_config, Msg}}) ->
    status(io_lib:format("Invalid Configuration: ~p~n", [Msg])).

-spec status(string()) -> status().
status(Str) ->
    [riak_cli_status:alert([riak_cli_status:text(Str)])].
