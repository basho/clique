-module(riak_cli_manager).

-behaviour(gen_server).

%% gen_server api and callbacks
-export([start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

%% API
-export([register_command/5,
         register_config/2,
         run/1,
         write_status/1]).

-define(cmd_table, riak_cli_commands).
-define(config_table, riak_cli_config).
-define(schema_table, riak_cli_schema).

-record(state, {}).

-type err() :: {error, term()}.
-type kvpair() :: {term(), term()}.
-type str_kvpairs() :: [{string(), string()}].
-type flags() :: [{atom(), term()}].
%%-type app_config() :: [{atom(), [kvpair()]}].
-type app_config() :: [proplists:property()].
-type spec() :: {atom(), [kvpair()]}.
-type keyspecs() :: [spec()].
-type flagspecs() ::[spec()].

%% @doc Register configuration callbacks for a given config key
-spec register_config([string()], fun()) -> true.
register_config(Key, Callback) ->
    ets:insert(?config_table, {Key, Callback}).

%% @doc Register a cli command (i.e.: "riak-admin handoff status")
-spec register_command([string()], string(), list(), list(), fun()) -> true.
register_command(Cmd, Description, Keys, Flags, Fun) ->
    ets:insert(?cmd_table, {Cmd, Description, Keys, Flags, Fun}).

write_status(Status) ->
    Output = riak_:write(Status),
    io:format("~s", [Output]),
    ok.

-spec run([string()]) -> ok | err().
run([_Script, "set" | Args]) ->
    run_set(Args);
run([_Script, "show" | Args]) ->
    run_show(Args);
run(Cmd) ->
    M0 = match(Cmd, ?cmd_table),
    M1 = parse(M0),
    M2 = validate(M1),
    run_command(M2).

-spec run_command(err()) -> err();
                 ({fun(), [kvpair()], [kvpair()]})-> ok | err().
run_command({error, _}=E) ->
    print_error(E),
    E;
run_command({Fun, Args, Flags}) ->
    Fun(Args, Flags).

-spec run_set([string()]) -> ok | err().
run_set(ArgsAndFlags) ->
    M1 = parse(ArgsAndFlags),
    M2 = get_config(M1),
    M3 = set_config(M2),
    case run_callback(M3) of
        {error, _}=E ->
            print_error(E);
        _ ->
            ok
    end.

%% TODO: This doesn't work for keys with translations.
%% This should almost certainly show the riak_conf value. For now it only works
%% for 1:1 mappings because of we don't save the user value AFAIK.
%% TODO: clean this function up
-spec run_show([string()]) -> ok | err().
run_show(KeysAndFlags) ->
    {Keys0, _Flags0} = lists:splitwith(fun is_not_flag/1, KeysAndFlags),
    Keys = [cuttlefish_variable:tokenize(K) || K <- Keys0],
    [{schema, Schema}] = ets:lookup(?schema_table, schema),
    {_Translations, Mappings0, _Validators} = Schema,
    Mappings = valid_mappings(Keys, Mappings0),
    case length(Mappings) =:= length(Keys) of
        false ->
            Invalid = invalid_keys(Keys, Mappings),
            io:format("Invalid Config Keys: ~p", [Invalid]),
            {error, {invalid_config_keys, Invalid}};
        true ->
            EnvStrs = [{element(2, M), element(3, M)} || M <- Mappings],

            AppAndKeys = [string:tokens(S, ".") || S <- EnvStrs],
            AppKeyPairs = [{list_to_atom(App), list_to_atom(Key)} || [App, Key] <-
                AppAndKeys],
            %% TODO: Extend this to handle --node and --all flags
            Rows = [[begin
                        {ok, Val} = application:get_env(App, Key),
                        Val
                     end || {App, Key} <- AppKeyPairs]],
            Status = [{table, Keys0, Rows}],
            write_status(Status)
    end.


valid_mappings(Keys, Mappings) ->
    lists:filter(fun(Mapping) ->
                     Key = element(2, Mapping),
                     lists:member(Key, Keys)
                 end, Mappings).

invalid_keys(Keys, Mappings) ->
    lists:filter(fun(Key) ->
                    not lists:keymember(Key, 2, Mappings)
                 end, Keys).

-spec run_callback(err()) -> err();
                  ([{[string()], string()}]) -> ok | err().
run_callback({error, _}=E) ->
    E;
run_callback({Args, Flags}) ->
    KVFuns = lists:foldl(fun({K, V}, Acc) ->
                             case ets:lookup(?config_table, K) of
                                 [{K, F}] ->
                                     [{K, V, F} | Acc];
                                 [] ->
                                     Acc
                             end
                         end, [], Args),
    [F(K, V, Flags) || {K, V, F} <- KVFuns].

-spec get_config(err()) -> err();
                ({str_kvpairs(), str_kvpairs()}) ->
                    {app_config(), str_kvpairs(), flags()} | err().
get_config({error, _}=E) ->
    E;
get_config({Args, Flags0}) ->
    [{schema, Schema}] = ets:lookup(?schema_table, schema),
    Conf = [{cuttlefish_variable:tokenize(K), V} || {K, V} <- Args],
    case cuttlefish_generator:minimal_map(Schema, Conf) of
        {error, _, Msg} ->
            {error, {invalid_config, Msg}};
        AppConfig ->
            case validate_flags(config_flags(), Flags0) of
                {error, _}=E ->
                    E;
                Flags ->
                    {AppConfig, Conf, Flags}
            end
    end.

-spec set_config(err()) -> err();
      ({app_config(), str_kvpairs(), flags()}) -> {[kvpair()], flags()}| err().
set_config({error, _}=E) ->
    E;
set_config({AppConfig, Args, Flags}) ->
    case set_app_config(AppConfig, Flags) of
        ok ->
            {Args, Flags};
        E ->
            E
    end.

-spec set_app_config(app_config(), flags()) -> ok | err().
set_app_config(AppConfig, []) ->
    set_local_app_config(AppConfig);
set_app_config(AppConfig, Flags) when length(Flags) =:= 1 ->
    [{Key, Val}] = Flags,
    case Key of
        node -> set_remote_app_config(AppConfig, Val);
        all -> set_remote_app_config(AppConfig)
    end;
set_app_config(_AppConfig, _Flags) ->
    Msg = "Cannot use --all(-a) and --node(-n) at the same time",
    io:format("Error: ~p~n", [Msg]),
    {error, {invalid_flag_combination, Msg}}.

-spec set_local_app_config(app_config()) -> ok.
set_local_app_config(AppConfig) ->
    _ = [application:set_env(App, Key, Val) || {App, Settings} <- AppConfig,
                                               {Key, Val} <- Settings],
    ok.

-spec set_remote_app_config(app_config(), node()) -> ok.
set_remote_app_config(AppConfig, Node) ->
    Fun = set_local_app_config,
    case safe_rpc(Node, ?MODULE, Fun, [AppConfig]) of
        {badrpc, rpc_process_down} ->
            io:format("Error: Node ~p Down~n", [Node]),
            ok;
        _ ->
            ok
    end.

-spec set_remote_app_config(app_config()) -> ok.
set_remote_app_config(AppConfig) ->
    io:format("Setting config across the cluster~n", []),
    {_, Down} = rpc:multicall(nodes(),
                              ?MODULE,
                              set_local_app_config,
                              [AppConfig],
                              60000),
    (Down == []) orelse io:format("Failed to set config for: ~p~n", [Down]),
    ok.

-spec config_flags() -> flags().
config_flags() ->
    [{node, [{shortname, "n"},
             {longname, "node"},
             {typecast, fun list_to_atom/1},
             {description,
                 "The node to apply the operation on"}]},

     {all, [{shortname, "a"},
            {longname, "all"},
            {description,
                "Apply the operation to all nodes in the cluster"}]}].

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Note that this gen_server only exists to create ets tables and keep them
%% around indefinitely.
init([]) ->
    _ = ets:new(?cmd_table, [public, named_table]),
    _ = ets:new(?config_table, [public, named_table]),
    _ = ets:new(?schema_table, [public, named_table]),
    SchemaFiles = filelib:wildcard(code:lib_dir() ++ "/*.schema"),
    Schema = cuttlefish_schema:files(SchemaFiles),
    true = ets:insert(?schema_table, {schema, Schema}),
    {ok, #state{}}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.
handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Msg, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec print_error(err()) -> ok.
print_error({error, no_matching_spec}) ->
    io:format("Invalid Command~n");
print_error({error, {invalid_flag, Str}}) ->
    io:format("Invalid Flag: ~p~n", [Str]);
print_error({error, {invalid_action, Str}}) ->
    io:format("Invalid Action: ~p~n", [Str]);
print_error({error, invalid_number_of_args}) ->
    io:format("Invalid number of arguments~n");
print_error({error, {invalid_argument, Str}}) ->
    io:format("Invalid argument: ~p~n", [Str]);
print_error({error, {invalid_flags, Flags}}) ->
    io:format("Invalid Flags: ~p~n", [Flags]);
print_error({error, {invalid_flag_value, {Name, Val}}}) ->
    io:format("Invalid value: ~p for flag: ~p~n", [Val, Name]);
print_error({error, {invalid_value, Val}}) ->
    io:format("Invalid value: ~p~n", [Val]);
print_error({error, {invalid_kv_arg, Arg}}) ->
    io:format("Not a Key/Value argument of format: ~p=<Value>: ~n", [Arg]);
print_error({error, {too_many_equal_signs, Arg}}) ->
    io:format("Too Many Equal Signs in Argument: ~p~n", [Arg]);
print_error({error, {invalid_config, Msg}}) ->
    io:format("Invalid Configuration: ~p~n", [Msg]).

-spec match([list()], atom() | ets:tid()) -> {tuple(), list()} | {error, no_matching_spec}.
match(Cmd0, Table) ->
    {Cmd, Args} = split_command(Cmd0),
    case ets:lookup(Table, Cmd) of
        [Spec] ->
            {Spec, Args};
        [] ->
            {error, no_matching_spec}
    end.

-spec split_command([list()]) -> {list(), list()}.
split_command(Cmd0) ->
    lists:splitwith(fun(Str) ->
                        is_not_kv_arg(Str) andalso is_not_flag(Str)
                    end, Cmd0).

-spec parse(err()) -> err();
           ([string()]) -> {str_kvpairs(), str_kvpairs()} | err();
           ({tuple(), [string()]}) ->
               {tuple(), str_kvpairs(), str_kvpairs()} | err().
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

-spec parse_kv_args([string()]) -> err() | str_kvpairs().
parse_kv_args(Args) ->
    parse_kv_args(Args, []).

%% All args must be k/v args!
-spec parse_kv_args([string()], str_kvpairs()) -> err() | str_kvpairs().
parse_kv_args([], Acc) ->
    Acc;
parse_kv_args([Arg | Args], Acc) ->
    case string:tokens(Arg, "=") of
        [Key, Val] ->
            parse_kv_args(Args, [{Key, Val} | Acc]);
        [Key] ->
            {error, {invalid_kv_arg, Key}};
        _ ->
            {error, {too_many_equal_signs, Arg}}
    end.


-spec parse_flags([string()]) -> err() | str_kvpairs().
parse_flags(Flags) ->
    parse_flags(Flags, [], []).

-spec parse_flags([string()], list(), [kvpair()]) -> [kvpair()] | err().
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

-spec validate(err()) -> err();
              ({tuple(), [kvpair()], [flags()]}) ->
                  err() | {fun(), [kvpair()], flags()}.
validate({error, _}=E) ->
    E;
validate({Spec, Args0, Flags0}) ->
    {_Cmd, _Description, KeySpecs, FlagSpecs, Callback} = Spec,
    case validate_args(KeySpecs, Args0) of
        {error, _}=E ->
            E;
        Args ->
            case validate_flags(FlagSpecs, Flags0) of
                {error, _}=E ->
                    E;
                Flags ->
                    {Callback, Args, Flags}
            end
    end.

-spec validate_args(keyspecs(), [kvpair()]) -> err() | [kvpair()].
validate_args(KeySpecs, Args) ->
    convert_args(KeySpecs, Args, []).

-spec convert_args(keyspecs(), [kvpair()], [kvpair()]) -> err() | [kvpair()].
convert_args([], [], Acc) ->
    Acc;
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

-spec convert_arg([{atom(), term()}], string(), string()) -> term().
convert_arg(Spec, Key, Val) ->
    {typecast, Fun} = lists:keyfind(typecast, 1, Spec),
    try
        Fun(Val)
    catch error:badarg ->
        {error, {invalid_argument, {Key, Val}}}
    end.

-spec validate_flags(flagspecs(), [kvpair()]) -> err() | flags().
validate_flags(FlagSpecs, Flags) ->
    convert_flags(FlagSpecs, Flags, []).

-spec convert_flags(flagspecs(), [kvpair()], flags()) -> err() | flags().
convert_flags([], [], Acc) ->
    Acc;
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

-spec convert_flag([{atom(), term()}], string(), string()) -> err() | term().
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

%% @doc Wraps an rpc:call/4 in a try/catch to handle the case where the
%%      'rex' process is not running on the remote node. This is safe in
%%      the sense that it won't crash the calling process if the rex
%%      process is down.
-spec safe_rpc(Node :: node(), Module :: atom(), Function :: atom(), Args :: [any()]) -> {'badrpc', any()} | any().
safe_rpc(Node, Module, Function, Args) ->
    try rpc:call(Node, Module, Function, Args) of
        Result ->
            Result
    catch
        exit:{noproc, _NoProcDetails} ->
            {badrpc, rpc_process_down}
    end.
