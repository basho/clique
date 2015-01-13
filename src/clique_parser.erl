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
-module(clique_parser).

%% API
-export([parse/1,
         parse_flags/1,
         extract_global_flags/1,
         validate/1,
         validate_flags/2,
         is_not_kv_arg/1,
         is_not_flag/1]).

-type err() :: {error, term()}.
-type flags() :: [{atom() | char(), term()}].
-type proplist() :: [{atom(), term()}].

%% TODO: A spec should probably just be a record
-type spec() :: {atom(), proplist()}.
-type keyspecs() :: '_' | [spec()].
-type flagspecs() :: [spec()].

-spec parse(err()) -> err();
           ([string()]) -> {proplist(), flags()} | err();
           ({tuple(), [string()]}) ->
               {tuple(), proplist(), flags()} | err().
parse({error, _}=E) ->
    E;
parse({Spec, ArgsAndFlags}) ->
    case parse(ArgsAndFlags) of
        {error, _}=E ->
            E;
        {Args, Flags} ->
            {Spec, Args, Flags}
    end;
parse(ArgsAndFlags) ->
    %% Positional key/value args always come before flags in our cli
    {Args0, Flags0} = lists:splitwith(fun is_not_flag/1, ArgsAndFlags),
    case parse_kv_args(Args0) of
        {error, _}=E ->
            E;
        Args ->
            case parse_flags(Flags0) of
                {error, _}=E ->
                    E;
                Flags ->
                    {Args, Flags}
            end
    end.

-spec parse_kv_args([string()]) -> err() | proplist().
parse_kv_args(Args) ->
    parse_kv_args(Args, []).

%% All args must be k/v args!
-spec parse_kv_args([string()], proplist()) -> err() | proplist().
parse_kv_args([], Acc) ->
    Acc;
parse_kv_args([Arg | Args], Acc) ->
    case string:tokens(Arg, "=") of
        [Key, Val] ->
            parse_kv_args(Args, [{list_to_atom(Key), Val} | Acc]);
        [Key] ->
            {error, {invalid_kv_arg, Key}};
        _ ->
            {error, {too_many_equal_signs, Arg}}
    end.


-spec parse_flags([string()]) -> err() | flags().
parse_flags(Flags) ->
    parse_flags(Flags, [], []).

-spec parse_flags([string()], list(), proplist()) -> proplist() | err().
parse_flags([], [], Acc) ->
    Acc;
parse_flags([], [Flag], Acc) ->
    [{Flag, undefined} | Acc];
parse_flags(["--"++Long | T], [], Acc) ->
    case string:tokens(Long,"=") of
        [Flag, Val] ->
            parse_flags(T, [], [{list_to_atom(Flag), Val} | Acc]);
        [Flag] ->
            parse_flags(T, [list_to_atom(Flag)], Acc)
    end;
parse_flags(["--"++_Long | _T]=Flags, [Flag], Acc) ->
    parse_flags(Flags, [], [{Flag, undefined} | Acc]);
parse_flags([[$-,Short] | T], [], Acc) ->
    parse_flags(T, [Short], Acc);
parse_flags([[$-,Short] | T], [Flag], Acc) ->
    parse_flags(T, [Short], [{Flag, undefined} | Acc]);
parse_flags([[$-,Short | Arg] | T], [], Acc) ->
    parse_flags(T, [], [{Short, Arg} | Acc]);
parse_flags([[$-,Short | Arg] | T], [Flag], Acc) ->
    parse_flags(T, [], [{Short, Arg}, {Flag, undefined} | Acc]);
parse_flags([Val | T], [Flag], Acc) ->
    parse_flags(T, [], [{Flag, Val} | Acc]);
parse_flags([Val | _T], [], _Acc) ->
    {error, {invalid_flag, Val}}.

%% TODO: If this gets more complicated, write out a function to extract
%% the flag names from ?GFLAG_SPECS instead of hand-coding it in ?GLOBAL_FLAGS
-define(GLOBAL_FLAGS, [$h, help, format]).
-define(GFLAG_SPECS, [{help, [{shortname, "h"},
                              {longname, "help"}]},
                      {format, [{longname, "format"}]}]).
%% @doc Extracts a list of globally applicable flags (e.g. --help) from the
%% the original command.
-spec extract_global_flags(err()) -> err();
                          ({tuple(), proplist(), flags()}) ->
                              {tuple(), proplist(), flags(), flags()}.
extract_global_flags({error, _} = E) ->
    E;
extract_global_flags({Spec, Args, Flags0}) ->
    PartFun = fun({K, _V}) -> lists:member(K, ?GLOBAL_FLAGS) end,
    {GlobalFlags0, Flags} = lists:partition(PartFun, Flags0),
    GlobalFlags = validate_flags(?GFLAG_SPECS, GlobalFlags0),
    {Spec, Args, Flags, GlobalFlags}.

-spec validate(err()) -> err();
              ({tuple(), proplist(), [flags()]}) ->
                  err() | {fun(), proplist(), flags()}.
validate({error, _}=E) ->
    E;
validate({Spec, Args0, Flags0, GlobalFlags}) ->
    {_Cmd, KeySpecs, FlagSpecs, Callback} = Spec,
    case validate_args(KeySpecs, Args0) of
        {error, _}=E ->
            E;
        Args ->
            case validate_flags(FlagSpecs, Flags0) of
                {error, _}=E ->
                    E;
                Flags ->
                    {Callback, Args, Flags, GlobalFlags}
            end
    end.

-spec validate_args(keyspecs(), proplist()) -> err() | proplist().
validate_args('_', Args) ->
    Args;
validate_args(KeySpecs, Args) ->
    convert_args(KeySpecs, Args, []).

-spec convert_args(keyspecs(), proplist(), proplist()) -> err() | proplist().
convert_args(_KeySpec, [], Acc) ->
    Acc;
convert_args([], Args, _Acc) ->
    {error, {invalid_args, Args}};
convert_args(KeySpecs, [{Key, Val0} | Args], Acc) ->
    case lists:keyfind(Key, 1, KeySpecs) of
        false ->
            {error, {invalid_key, Key}};
        {Key, Spec} ->
            case convert_arg(Spec, Key, Val0) of
                {error, _}=E ->
                    E;
                Val ->
                    convert_args(KeySpecs, Args, [{Key, Val} | Acc])
            end
    end.

-spec convert_arg(proplist(), atom(), string()) -> err() | term().
convert_arg(Spec, Key, Val) ->
    {typecast, Fun} = lists:keyfind(typecast, 1, Spec),
    try
        Fun(Val)
    catch error:badarg ->
        {error, {invalid_argument, {Key, Val}}}
    end.

-spec validate_flags(flagspecs(), proplist()) -> err() | flags().
validate_flags(FlagSpecs, Flags) ->
    convert_flags(FlagSpecs, Flags, []).

-spec convert_flags(flagspecs(), proplist(), flags()) -> err() | flags().
convert_flags(_FlagSpecs, [], Acc) ->
    Acc;
convert_flags([], Provided, _Acc) ->
    Invalid = [Flag || {Flag, _} <- Provided],
    {error, {invalid_flags, Invalid}};
convert_flags(FlagSpecs, [{Key, Val0} | Flags], Acc) ->
    case lists:keyfind(Key, 1, FlagSpecs) of
        false ->
            %% We may have been passed a -short option instead of a --long option
            case find_shortname_key(Key, FlagSpecs) of
                {error, _}=E ->
                    E;
                NewKey ->
                    %% We just want to replace the shortname with a valid key
                    %% (atom of longname) and call the same function again.
                    convert_flags(FlagSpecs, [{NewKey, Val0} | Flags], Acc)
            end;
        {Key, Spec} ->
            case convert_flag(Spec, Key, Val0) of
                {error, _}=E ->
                    E;
                Val ->
                    convert_flags(FlagSpecs, Flags, [{Key, Val} | Acc])
            end
    end.

-spec convert_flag(proplist(), atom(), string()) -> err() | term().
convert_flag(Spec, Key, Val) ->
    %% Flags don't necessarily have values, in which case Val is undefined here.
    %% Additionally, flag values can also be strings and not have typecast funs.
    %% It's not incorrect, so just return the value in that case.
    case lists:keyfind(typecast, 1, Spec) of
        false ->
            Val;
        {typecast, Fun} ->
            try
                Fun(Val)
            catch error:badarg ->
                {error, {invalid_flag, {Key, Val}}}
            end
    end.

-spec find_shortname_key(char(), flagspecs()) -> err() | atom().
find_shortname_key(ShortVal, FlagSpecs) ->
    %% Make it a string instead of an int
    Short = [ShortVal],
    Error = {error, {invalid_flag, Short}},
    lists:foldl(fun({Key, Props}, Acc) ->
                    case lists:member({shortname, Short}, Props) of
                        true ->
                            Key;
                        false ->
                            Acc
                    end
                end, Error, FlagSpecs).

-spec is_not_kv_arg(string()) -> boolean().
is_not_kv_arg("-"++_Str) ->
    true;
is_not_kv_arg(Str) ->
    case lists:member($=, Str) of
        true ->
            false;
        false ->
            true
    end.

-spec is_not_flag(string()) -> boolean().
is_not_flag(Str) ->
    case lists:prefix("-", Str) of
        true ->
            try
                %% negative integers are arguments
                _ = list_to_integer(Str),
                true
            catch error:badarg ->
                false
            end;
        false ->
            true
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

spec() ->
    Cmd = ["riak-admin", "test", "something"],
    KeySpecs = [{sample_size, [{typecast, fun list_to_integer/1}]}],
    FlagSpecs = [{node, [{shortname, "n"},
                         {longname, "node"},
                         {typecast, fun list_to_atom/1}]},
                 {force, [{shortname, "f"},
                          {longname, "force"}]}],
    Callback = undefined,
    {Cmd, KeySpecs, FlagSpecs, Callback}.

parse_valid_flag_test() ->
    Spec = spec(),
    Node = "dev2@127.0.0.1",
    ArgsAndFlags = ["-n", Node],
    {Spec, Args, Flags} = parse({Spec, ArgsAndFlags}),
    ?assertEqual(Args, []),
    ?assertEqual(Flags, [{$n, Node}]).

parse_valid_args_and_flag_test() ->
    Spec = spec(),
    Node = "dev2@127.0.0.1",
    ArgsAndFlags = ["key=value", "-n", Node],
    {Spec, Args, Flags} = parse({Spec, ArgsAndFlags}),
    ?assertEqual(Args, [{key, "value"}]),
    ?assertEqual(Flags, [{$n, Node}]).

%% All arguments must be of type k=v
parse_invalid_kv_arg_test() ->
    Spec = spec(),
    Args = ["ayo"],
    ?assertMatch({error, _}, parse({Spec, Args})).

%% This succeeds, because we aren't validating the flag, just parsing
%% Note: Short flags get parsed into tuples with their character as first elem
%% Long flags get translated to atoms in the first elem of the tuple
parse_valueless_flags_test() ->
    Spec = spec(),
    Args = ["-f", "--do-something"],
    {Spec, _, Flags} = parse({Spec, Args}),
    %% Flags with no value, get the value undefined
    ?assert(lists:member({$f, undefined}, Flags)),
    ?assert(lists:member({'do-something', undefined}, Flags)).

validate_valid_short_flag_test() ->
    Spec = spec(),
    Args = [],
    Node = "dev2@127.0.0.1",
    Flags = [{$n, Node}, {$f, undefined}],
    {undefined, [], ConvertedFlags, []} = validate({Spec, Args, Flags, []}),
    ?assert(lists:member({node, 'dev2@127.0.0.1'}, ConvertedFlags)),
    ?assert(lists:member({force, undefined}, ConvertedFlags)).

validate_valid_long_flag_test() ->
    Spec = spec(),
    Args = [],
    Node = "dev2@127.0.0.1",
    Flags = [{node, Node}, {force, undefined}],
    {undefined, [], ConvertedFlags, []} = validate({Spec, Args, Flags, []}),
    ?assert(lists:member({node, 'dev2@127.0.0.1'}, ConvertedFlags)),
    ?assert(lists:member({force, undefined}, ConvertedFlags)).

validate_invalid_flags_test() ->
    Spec = spec(),
    Args = [],
    Node = "dev2@127.0.0.1",
    InvalidFlags = [{'some-flag', Node},
                    {$b, Node},
                    {$a, undefined}],
    [?assertMatch({error, _}, validate({Spec, Args, [F], []})) || F <- InvalidFlags].

validate_valid_args_test() ->
    Spec = spec(),
    Args = [{sample_size, "5"}],
    {undefined, ConvertedArgs, [], []} = validate({Spec, Args, [], []}),
    ?assertEqual(ConvertedArgs, [{sample_size, 5}]).

validate_invalid_args_test() ->
    Spec = spec(),
    InvalidArgs = [{key, "value"}, {sample_size, "ayo"}],
    [?assertMatch({error, _}, validate({Spec, [A], [], []})) || A <- InvalidArgs].

-endif.
