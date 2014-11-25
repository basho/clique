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
-export([register_command/4,
         register_config/2,
         register_usage/2,
         register_node_finder/1,
         run/1,
         write_status/1]).

%% Callbacks for rpc calls
-export([get_local_env_status/2,
         get_local_env_vals/1,
         set_local_app_config/1]).

-define(cmd_table, riak_cli_commands).
-define(config_table, riak_cli_config).
-define(schema_table, riak_cli_schema).
-define(usage_table, riak_cli_usage).
-define(nodes_table, riak_cli_nodes).

-record(state, {}).

-type status() :: riak_cli_status:status().
-type proplist() :: [{atom(), term()}].
-type err() :: {error, term()}.
-type flags() :: [{atom() | char(), term()}].
-type app_config() :: proplist().
-type spec() :: {atom(), proplist()}.
-type keyspecs() :: [spec()].
-type flagspecs() ::[spec()].

-spec register_node_finder(fun()) -> true.
register_node_finder(Fun) ->
    ets:insert(?nodes_table, {nodes_fun, Fun}).

%% @doc Register configuration callbacks for a given config key
-spec register_config([string()], fun()) -> true.
register_config(Key, Callback) ->
    ets:insert(?config_table, {Key, Callback}).

%% @doc Register a cli command (i.e.: "riak-admin handoff status")
-spec register_command([string()], list(), list(), fun()) -> true.
register_command(Cmd, Keys, Flags, Fun) ->
    ets:insert(?cmd_table, {Cmd, Keys, Flags, Fun}).

%% @doc Register usage for a given command sequence. Lookups are by longest
%% match.
-spec register_usage([string()], iolist()) -> true.
register_usage(Cmd, Usage0) ->
    Usage = ["Usage: ", Usage0],
    ets:insert(?usage_table, {Cmd, Usage}).

write_status(Status) ->
    Output = riak_cli_writer:write(Status),
    io:format("~s", [Output]),
    ok.

-spec run([string()]) -> ok | err().
run([_Script, "set" | Args]) ->
    run_set(Args);
run([_Script, "show" | Args]) ->
    run_show(Args);
run(Cmd0) ->
    case is_help(Cmd0) of
        {ok, Cmd} ->
            print_usage(Cmd);
        _ ->
            M0 = match(Cmd0, ?cmd_table),
            M1 = parse(M0),
            M2 = validate(M1),
            run_command(M2)
    end.

%% @doc Help flags always comes at the end of the command
-spec is_help(iolist()) -> {ok, iolist()} | false.
is_help(Str) ->
    [H | T] = lists:reverse(Str),
    case H =:= "--help" orelse H =:= "-h" of
        true ->
            {ok, lists:reverse(T)};
        false ->
            false
    end.

-spec print_usage(iolist()) -> ok.
print_usage(Cmd) ->
    Usage = case find_usage(Cmd) of
                {error, Error} ->
                    Error;
                Usage2 ->
                    Usage2
            end,
    io:format("~s", [Usage]).

-spec find_usage(iolist()) -> iolist() | err().
find_usage([]) ->
    {error, "Error: Usage information not found for the given command\n\n"};
find_usage(Cmd) ->
    case ets:lookup(?usage_table, Cmd) of
        [{Cmd, Usage}] ->
            Usage;
        [] ->
            Cmd2 = lists:reverse(tl(lists:reverse(Cmd))),
            find_usage(Cmd2)
    end.

-spec run_command(err()) -> err();
                 ({fun(), proplist(), proplist()})-> ok | err().
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
%% This should almost certainly show the riak.conf value. For now it only works
%% for 1:1 mappings because of we don't save the user value AFAIK.
-spec run_show([string()]) -> ok | err().
run_show(KeysAndFlags) ->
    case get_valid_mappings(KeysAndFlags) of
        {error, _}=E ->
            print_error(E);
        {Keys, Mappings, Flags0}->
            AppKeyPairs = get_env_keys(Mappings),
            case validate_flags(config_flags(), Flags0) of
                {error, _}=E ->
                    print_error(E);
                Flags ->
                    Status = get_env_status(Keys, AppKeyPairs, Flags),
                    write_status(Status)
            end
    end.

-spec get_env_status([string()], [{atom(), atom()}], flags()) -> status().
get_env_status(Keys, AppKeyPairs, []) ->
    get_local_env_status(Keys, AppKeyPairs);
get_env_status(Keys, AppKeyPairs, Flags) when length(Flags) =:= 1 ->
    [{Key, Val}] = Flags,
    case Key of
        node -> get_remote_env_status(Keys, AppKeyPairs, Val);
        all -> get_remote_env_status(Keys, AppKeyPairs)
    end;
get_env_status(_Keys, _AppKeyPairs, _Flags) ->
    print_error(app_config_flags_error()),
    [].

-spec get_local_env_status([string()], [{atom(), atom()}]) -> status().
get_local_env_status(Keys, AppKeyPairs) ->
    Row = get_local_env_vals(AppKeyPairs),
    [riak_cli_status:table([lists:zip(["Node" | Keys], Row)])].

-spec get_local_env_vals([{atom(), atom()}]) -> list().
get_local_env_vals(AppKeyPairs) ->
    Vals = [begin
                {ok, Val} = application:get_env(App, Key),
                Val
            end || {App, Key} <- AppKeyPairs],
    [node() | Vals].

-spec get_remote_env_status([string()], [{atom(), atom()}], node()) -> status().
get_remote_env_status(Keys, AppKeyPairs, Node) ->
    case safe_rpc(Node, ?MODULE, get_local_env_status, [Keys, AppKeyPairs]) of
        {badrpc, rpc_process_down} ->
            io:format("Error: Node ~p Down~n", [Node]),
            [];
        Status ->
            Status
    end.

-spec get_nodes() -> [node()].
get_nodes() ->
    [{nodes_fun, Fun}] = ets:lookup(?nodes_table, nodes_fun),
    Fun().

-spec get_remote_env_status([string()], [{atom(), atom()}]) -> status().
get_remote_env_status(Keys0, AppKeyPairs) ->
    Nodes = get_nodes(),
    {Rows0, Down} = rpc:multicall(Nodes,
                                  ?MODULE,
                                  get_local_env_vals,
                                  [AppKeyPairs],
                                  60000),
    Keys = ["Node" | Keys0],
    Rows = [lists:zip(Keys, Row) || Row <- Rows0],
    Table = riak_cli_status:table(Rows),
    case (Down == []) of
        false ->
            Text = io_lib:format("Failed to get config for: ~p~n", [Down]),
            Alert = riak_cli_status:alert([riak_cli_status:text(Text)]),
            [Table, Alert];
        true ->
            [Table]
    end.

-spec app_config_flags_error() -> err().
app_config_flags_error() ->
    Msg = "Cannot use --all(-a) and --node(-n) at the same time",
    {error, {invalid_flag_combination, Msg}}.

-spec get_env_keys(list(tuple())) -> [{atom(), atom()}].
get_env_keys(Mappings) ->
    EnvStrs = [element(3, M) || M <- Mappings],
    AppAndKeys = [string:tokens(S, ".") || S <- EnvStrs],
    [{list_to_atom(App), list_to_atom(Key)} || [App, Key] <-  AppAndKeys].

-spec get_valid_mappings([string()]) -> err() | {[string()], list(tuple()), flags()}.
get_valid_mappings(KeysAndFlags) ->
    {Keys0, Flags0} = lists:splitwith(fun is_not_flag/1, KeysAndFlags),
    Keys = [cuttlefish_variable:tokenize(K) || K <- Keys0],
    [{schema, Schema}] = ets:lookup(?schema_table, schema),
    {_Translations, Mappings0, _Validators} = Schema,
    Mappings = valid_mappings(Keys, Mappings0),
    case length(Mappings) =:= length(Keys) of
        false ->
            Invalid = invalid_keys(Keys, Mappings),
            {error, {invalid_config_keys, Invalid}};
        true ->
            case parse_flags(Flags0) of
                {error, _}=E ->
                    E;
                Flags ->
                    {Keys0, Mappings, Flags}
            end
    end.


-spec valid_mappings([string()], [tuple()]) -> [tuple()].
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
                ({proplist(), flags()}) ->
                    {app_config(), proplist(), flags()} | err().
get_config({error, _}=E) ->
    E;
get_config({Args, Flags0}) ->
    [{schema, Schema}] = ets:lookup(?schema_table, schema),
    Conf = [{cuttlefish_variable:tokenize(atom_to_list(K)), V} || {K, V} <- Args],
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
      ({app_config(), proplist(), flags()}) -> {proplist(), flags()}| err().
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
    app_config_flags_error().

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
        ok ->
            ok
    end.

-spec set_remote_app_config(app_config()) -> ok.
set_remote_app_config(AppConfig) ->
    io:format("Setting config across the cluster~n", []),
    {_, Down} = rpc:multicall(get_nodes(),
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
    _ = ets:new(?usage_table, [public, named_table]),
    _ = ets:new(?nodes_table, [public, named_table]),
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
print_error({error, {no_matching_spec, Cmd}}) ->
    case find_usage(Cmd) of
        {error, _} ->
            io:format("Invalid Command~n");
        Usage ->
            io:format("~s", [Usage])
    end;
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
print_error({error, {invalid_flag_combination, Msg}}) ->
    io:format("Error: ~s~n", [Msg]);
print_error({error, {invalid_value, Val}}) ->
    io:format("Invalid value: ~p~n", [Val]);
print_error({error, {invalid_kv_arg, Arg}}) ->
    io:format("Not a Key/Value argument of format: ~p=<Value>: ~n", [Arg]);
print_error({error, {too_many_equal_signs, Arg}}) ->
    io:format("Too Many Equal Signs in Argument: ~p~n", [Arg]);
print_error({error, {invalid_config_keys, Invalid}}) ->
    io:format("Invalid Config Keys: ~p~n", [Invalid]);
print_error({error, {invalid_config, Msg}}) ->
    io:format("Invalid Configuration: ~p~n", [Msg]).

-spec match([list()], atom() | ets:tid()) -> {tuple(), list()} | {error, no_matching_spec}.
match(Cmd0, Table) ->
    {Cmd, Args} = split_command(Cmd0),
    case ets:lookup(Table, Cmd) of
        [Spec] ->
            {Spec, Args};
        [] ->
            {error, {no_matching_spec, Cmd0}}
    end.

-spec split_command([list()]) -> {list(), list()}.
split_command(Cmd0) ->
    lists:splitwith(fun(Str) ->
                        is_not_kv_arg(Str) andalso is_not_flag(Str)
                    end, Cmd0).

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

-spec validate(err()) -> err();
              ({tuple(), proplist(), [flags()]}) ->
                  err() | {fun(), proplist(), flags()}.
validate({error, _}=E) ->
    E;
validate({Spec, Args0, Flags0}) ->
    {_Cmd, KeySpecs, FlagSpecs, Callback} = Spec,
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

-spec validate_args(keyspecs(), proplist()) -> err() | proplist().
validate_args(KeySpecs, Args) ->
    convert_args(KeySpecs, Args, []).

-spec convert_args(keyspecs(), proplist(), proplist()) -> err() | proplist().
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
    {undefined, [], ConvertedFlags} = validate({Spec, Args, Flags}),
    ?assert(lists:member({node, 'dev2@127.0.0.1'}, ConvertedFlags)),
    ?assert(lists:member({force, undefined}, ConvertedFlags)).

validate_valid_long_flag_test() ->
    Spec = spec(),
    Args = [],
    Node = "dev2@127.0.0.1",
    Flags = [{node, Node}, {force, undefined}],
    {undefined, [], ConvertedFlags} = validate({Spec, Args, Flags}),
    ?assert(lists:member({node, 'dev2@127.0.0.1'}, ConvertedFlags)),
    ?assert(lists:member({force, undefined}, ConvertedFlags)).

validate_invalid_flags_test() ->
    Spec = spec(),
    Args = [],
    Node = "dev2@127.0.0.1",
    InvalidFlags = [{'some-flag', Node},
                    {$b, Node},
                    {$a, undefined}],
    [?assertMatch({error, _}, validate({Spec, Args, [F]})) || F <- InvalidFlags].

validate_valid_args_test() ->
    Spec = spec(),
    Args = [{sample_size, "5"}],
    {undefined, ConvertedArgs, []} = validate({Spec, Args, []}),
    ?assertEqual(ConvertedArgs, [{sample_size, 5}]).

validate_invalid_args_test() ->
    Spec = spec(),
    InvalidArgs = [{key, "value"}, {sample_size, "ayo"}],
    [?assertMatch({error, _}, validate({Spec, [A], []})) || A <- InvalidArgs].

-endif.
