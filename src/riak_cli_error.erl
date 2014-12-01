-module(riak_cli_error).

%% API
-export([print/1]).

-type err() :: {error, term()}.

-spec print(err()) -> ok.
print({error, {no_matching_spec, Cmd}}) ->
    case riak_cli_usage:find(Cmd) of
        {error, _} ->
            io:format("Invalid Command~n");
        Usage ->
            io:format("~s", [Usage])
    end;
print({error, {invalid_flag, Str}}) ->
    io:format("Invalid Flag: ~p~n", [Str]);
print({error, {invalid_action, Str}}) ->
    io:format("Invalid Action: ~p~n", [Str]);
print({error, invalid_number_of_args}) ->
    io:format("Invalid number of arguments~n");
print({error, {invalid_argument, Str}}) ->
    io:format("Invalid argument: ~p~n", [Str]);
print({error, {invalid_flags, Flags}}) ->
    io:format("Invalid Flags: ~p~n", [Flags]);
print({error, {invalid_flag_value, {Name, Val}}}) ->
    io:format("Invalid value: ~p for flag: ~p~n", [Val, Name]);
print({error, {invalid_flag_combination, Msg}}) ->
    io:format("Error: ~s~n", [Msg]);
print({error, {invalid_value, Val}}) ->
    io:format("Invalid value: ~p~n", [Val]);
print({error, {invalid_kv_arg, Arg}}) ->
    io:format("Not a Key/Value argument of format: ~p=<Value>: ~n", [Arg]);
print({error, {too_many_equal_signs, Arg}}) ->
    io:format("Too Many Equal Signs in Argument: ~p~n", [Arg]);
print({error, {invalid_config_keys, Invalid}}) ->
    io:format("Invalid Config Keys: ~p~n", [Invalid]);
print({error, {invalid_config, Msg}}) ->
    io:format("Invalid Configuration: ~p~n", [Msg]).
