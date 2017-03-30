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
-module(clique_config).
-include("clique_specs.hrl").

%% API
-export([init/0,
         register/2,
         register_formatter/2,
         config_flags/0,
         show/2,
         set/2,
         whitelist/1,
         describe/2,
         load_schema/1]).

%% Callbacks for rpc calls
-export([do_set/1,
         get_local_env_status/2,
         get_local_env_vals/2]).

-define(config_table, clique_config).
-define(schema_table, clique_schema).
-define(whitelist_table, clique_whitelist).
-define(formatter_table, clique_formatter).

-type err() :: {error, term()}.
-type status() :: clique_status:status().
-type proplist() :: [{atom(), term()}].
-type flagspecs() :: [spec()].
-type flags() :: proplist().
-type args() :: clique_parser:args().
-type conf() :: [{[string()], string()}].

-type envkey() :: {string(), {atom(), atom()}}.
-type cuttlefish_flag_spec() :: {flag, atom(), atom()}.
-type cuttlefish_flag_list() :: [undefined | cuttlefish_flag_spec()].

%% @doc Register configuration callbacks for a given config key
-spec register([string()], fun()) -> true.
register(Key, Callback) ->
    ets:insert(?config_table, {Key, Callback}).

%% @doc Register a pretty-print function for a given config key
-spec register_formatter([string()], fun()) -> true.
register_formatter(Key, Callback) ->
    ets:insert(?formatter_table, {Key, Callback}).

init() ->
    _ = ets:new(?config_table, [public, named_table]),
    _ = ets:new(?schema_table, [public, named_table]),
    _ = ets:new(?whitelist_table, [public, named_table]),
    _ = ets:new(?formatter_table, [public, named_table]),
    ok.

%% @doc Load Schemas into ets when given directories containing the *.schema files.
%% Note that this must be run before any registrations are made.
-spec load_schema([string()]) -> ok | {error, schema_files_not_found}.
load_schema(Directories) ->
    SchemaFiles = schema_paths(Directories),
    case SchemaFiles of
        [] ->
            {error, schema_files_not_found};
        _ ->
            Schema = cuttlefish_schema:files(SchemaFiles),
            true = ets:insert(?schema_table, {schema, Schema}),
            ok
    end.

-spec schema_paths([string()]) -> [string()].
schema_paths(Directories) ->
    lists:foldl(fun(Dir, Acc) ->
                    Files = filelib:wildcard(Dir ++ "/*.schema"),
                    Files ++ Acc
                end, [], Directories).

-spec show([string()], proplist()) -> clique_status:status() | err().
show(Args, Flags) ->
    case get_valid_mappings(Args) of
        {error, _}=E ->
            E;
        [] ->
            {error, show_no_args};
        KeyMappings ->
            EnvKeys = get_env_keys(KeyMappings),
            CuttlefishFlags = get_cuttlefish_flags(KeyMappings),
            get_env_status(EnvKeys, CuttlefishFlags, Flags)
    end.

-spec describe([string()], proplist()) -> clique_status:status() | err().
describe(Args, _Flags) ->
    case get_valid_mappings(Args) of
        {error, _}=E ->
            E;
        [] ->
            {error, describe_no_args};
        %% TODO: Do we want to allow any flags? --verbose maybe?
        KeyMappings ->
            [begin
                 Doc = cuttlefish_mapping:doc(M),
                 Name = cuttlefish_variable:format(cuttlefish_mapping:variable(M)),
                 clique_status:text(Name ++ ":\n  " ++ string:join(Doc,"\n  ") ++ "\n")
             end || {_, M} <- KeyMappings]
    end.

-spec set(proplist(), proplist()) -> status() | err().
set(Args, [{all, _}]) ->
    %% Done as an io:format instead of a status, so that the user is not totally
    %% left in the dark if the multicall ends up taking a while to finish:
    io:format("Setting config across the cluster~n", []),
    Nodes = clique_nodes:nodes(),
    {Results0, Down0} = rpc:multicall(Nodes, ?MODULE, do_set, [Args]),

    Results = [[{"Node", Node}, {"Node Down/Unreachable", false}, {"Result", Status}] ||
               {Node, Status} <- Results0],
    Down = [[{"Node", Node}, {"Node Down/Unreachable", true}, {"Result", "N/A"}] ||
            Node <- Down0],

    NodeStatuses = lists:sort(Down ++ Results),
    [clique_status:table(NodeStatuses)];
set(Args, [{node, Node}]) ->
    M1 = clique_nodes:safe_rpc(Node, ?MODULE, do_set, [Args]),
    return_set_status(M1, Node);
set(Args, []) ->
    M1 = do_set(Args),
    return_set_status(M1, node());
set(_Args, _Flags) ->
    app_config_flags_error().

return_set_status({error, _} = E, _Node) ->
    E;
return_set_status({badrpc, Reason}, Node) ->
    clique_error:badrpc_to_error(Node, Reason);
return_set_status({_, Result}, _Node) ->
    [clique_status:text(Result)].

do_set(Args) ->
    M1 = get_config(Args),
    M2 = set_config(M1),
    run_callback(M2).

%% @doc Whitelist settable cuttlefish variables. By default all variables are not settable.
-spec whitelist([string()]) -> ok | {error, {invalid_config_keys, [string()]}}.
whitelist(Keys) ->
    case get_valid_mappings(Keys) of
        {error, _}=E ->
            E;
        _ ->
            _ = [ets:insert(?whitelist_table, {Key}) || Key <- Keys],
            ok
    end.

-spec check_keys_in_whitelist([string()]) -> ok | {error, {config_not_settable, [string()]}}.
check_keys_in_whitelist(Keys) ->
    Invalid =lists:foldl(fun(K, Acc) ->
                             case ets:lookup(?whitelist_table, K) of
                                 [{_K}] ->
                                     Acc;
                                 [] ->
                                     [K | Acc]
                             end
                         end, [], Keys),
    case Invalid of
        [] -> ok;
        _ -> {error, {config_not_settable, Invalid}}
    end.

-spec get_env_status([envkey()], cuttlefish_flag_list(), flags()) -> status() | err().
get_env_status(EnvKeys, CuttlefishFlags, []) ->
    get_local_env_status(EnvKeys, CuttlefishFlags);
get_env_status(EnvKeys, CuttlefishFlags, Flags) when length(Flags) =:= 1 ->
    [{Key, Val}] = Flags,
    case Key of
        node -> get_remote_env_status(EnvKeys, CuttlefishFlags, Val);
        all -> get_remote_env_status(EnvKeys, CuttlefishFlags)
    end;
get_env_status(_EnvKeys, _CuttlefishFlags, _Flags) ->
    app_config_flags_error().

-spec get_local_env_status([envkey()], cuttlefish_flag_list()) -> status().
get_local_env_status(EnvKeys, CuttlefishFlags) ->
    Row = get_local_env_vals(EnvKeys, CuttlefishFlags),
    [clique_status:table([Row])].

-spec get_local_env_vals([envkey()], cuttlefish_flag_list()) -> list().
get_local_env_vals(EnvKeys, CuttlefishFlags) ->
    Vals = [begin
                {ok, Val} = application:get_env(App, Key),
                Val1 = case {CFlagSpec, Val} of
                           {{flag, TrueVal, _}, true} ->
                               TrueVal;
                           {{flag, _, FalseVal}, false} ->
                               FalseVal;
                           _ ->
                               Val
                       end,
                FormatterKey = cuttlefish_variable:tokenize(KeyStr),
                Val2 = case ets:lookup(?formatter_table, FormatterKey) of
                           [] ->
                               Val1;
                           [{_K, FormatterFun}] ->
                               FormatterFun(Val1)
                       end,
                {KeyStr, Val2}
            end || {{KeyStr, {App, Key}}, CFlagSpec} <- lists:zip(EnvKeys, CuttlefishFlags)],
    [{"node", node()} | Vals].

-spec get_remote_env_status([envkey()], cuttlefish_flag_list(), node()) -> status() | err().
get_remote_env_status(EnvKeys, CuttlefishFlags, Node) ->
    case clique_nodes:safe_rpc(Node, ?MODULE, get_local_env_status,
                               [EnvKeys, CuttlefishFlags]) of
        {badrpc, Reason} ->
            clique_error:badrpc_to_error(Node, Reason);
        Status ->
            Status
    end.

-spec get_remote_env_status([{atom(), atom()}], cuttlefish_flag_list()) -> status().
get_remote_env_status(EnvKeys, CuttlefishFlags) ->
    Nodes = clique_nodes:nodes(),
    {Rows, Down} = rpc:multicall(Nodes,
                                  ?MODULE,
                                  get_local_env_vals,
                                  [EnvKeys, CuttlefishFlags],
                                  60000),
    Table = clique_status:table(Rows),
    case (Down == []) of
        false ->
            Text = io_lib:format("Failed to get config for: ~p~n", [Down]),
            Alert = clique_status:alert([clique_status:text(Text)]),
            [Table, Alert];
        true ->
            [Table]
    end.


-spec run_callback(err()) -> err();
                  (conf()) -> {node, iolist()}.
run_callback({error, _}=E) ->
    E;
run_callback(Args) ->
    OutStrings = [run_callback(K, V, F) || {K, V} <- Args, {_, F} <- ets:lookup(?config_table, K)],
    Output = string:join(OutStrings, "\n"), %% TODO return multiple strings tagged with keys
    %% Tag the return value with our current node so we know
    %% where this result came from when we use multicall:
    {node(), Output}.

run_callback(K, V, F) ->
    KeyString = cuttlefish_variable:format(K),
    UpdateMsg = io_lib:format("~s set to ~p", [KeyString, V]),
    case F(K, V) of
        "" ->
            UpdateMsg;
        Output ->
            [UpdateMsg, $\n, Output]
    end.

-spec get_config(args()) -> err() | {args(), proplist(), conf()}.
get_config([]) ->
    {error, set_no_args};
get_config(Args) ->
    [{schema, Schema}] = ets:lookup(?schema_table, schema),
    Conf = [{cuttlefish_variable:tokenize(K), V} || {K, V} <- Args],
    case cuttlefish_generator:minimal_map(Schema, Conf) of
        {error, _, Msg} ->
            {error, {invalid_config, Msg}};
        AppConfig ->
            {Args, AppConfig, Conf}
    end.

-spec set_config(err()) -> err();
                ({args(), proplist(), conf()}) -> err() | conf().
set_config({error, _}=E) ->
    E;
set_config({Args, AppConfig, Conf}) ->
    Keys = [K || {K, _}  <- Args],
    case check_keys_in_whitelist(Keys) of
        ok ->
            _ = set_app_config(AppConfig),
            Conf;
        {error, _}=E ->
            E
    end.

-spec set_app_config(proplist()) -> _.
set_app_config(AppConfig) ->
    [application:set_env(App, Key, Val) || {App, Settings} <- AppConfig,
                                           {Key, Val} <- Settings].

-spec config_flags() -> flagspecs().
config_flags() ->
    [clique_spec:make({node, [{shortname, "n"},
                              {longname, "node"},
                              {typecast, fun clique_typecast:to_node/1},
                              {description,
                               "The node to apply the operation on"}]}),

     clique_spec:make({all, [{shortname, "a"},
                             {longname, "all"},
                             {description,
                              "Apply the operation to all nodes in the cluster"}]})].


-spec get_valid_mappings([string()]) -> err() | [{string(), cuttlefish_mapping:mapping()}].
get_valid_mappings(Keys0) ->
    Keys = [cuttlefish_variable:tokenize(K) || K <- Keys0],
    [{schema, Schema}] = ets:lookup(?schema_table, schema),
    {_Translations, Mappings0, _Validators} = Schema,
    KeyMappings0 = valid_mappings(Keys, Mappings0),
    KeyMappings = match_key_order(Keys0, KeyMappings0),
    case length(KeyMappings) =:= length(Keys) of
        false ->
            Invalid = invalid_keys(Keys, KeyMappings),
            {error, {invalid_config_keys, Invalid}};
        true ->
            KeyMappings
    end.

-spec valid_mappings([cuttlefish_variable:variable()], [cuttlefish_mapping:mapping()]) ->
    [{string(), cuttlefish_mapping:mapping()}].
valid_mappings(Keys, Mappings) ->
    lists:foldl(fun(Mapping, Acc) ->
                    Key = cuttlefish_mapping:variable(Mapping),
                    case lists:member(Key, Keys) of
                        true ->
                            Key2 = cuttlefish_variable:format(Key),
                            [{Key2, Mapping} | Acc];
                        false ->
                            Acc
                    end
                end, [], Mappings).

%% @doc Match the order of Keys in KeyMappings
match_key_order(Keys, KeyMappings) ->
    [lists:keyfind(Key, 1, KeyMappings) || Key <- Keys,
        lists:keyfind(Key, 1, KeyMappings) /= false].

-spec invalid_keys([cuttlefish_variable:variable()],
                   [{string(), cuttlefish_mapping:mapping()}]) -> [string()].
invalid_keys(Keys, KeyMappings) ->
    Valid = [cuttlefish_variable:tokenize(K) || {K, _M} <- KeyMappings],
    Invalid = Keys -- Valid,
    [cuttlefish_variable:format(I)++" " || I <- Invalid].

-spec get_env_keys([{string(), cuttlefish_mapping:mapping()}]) -> [envkey()].
get_env_keys(Mappings) ->
    KeyEnvStrs = [{Var, cuttlefish_mapping:mapping(M)} || {Var, M} <- Mappings],
    VarAppAndKeys = [{Var, string:tokens(S, ".")} || {Var, S} <- KeyEnvStrs],
    [{Var, {list_to_atom(App), list_to_atom(Key)}} || {Var, [App, Key]} <- VarAppAndKeys].

%% This is part of a minor hack we've added for correctly displaying config
%% values of type 'flag'. We pull out any relevant info from the mappings
%% about flag values, and then use it later on to convert true/false values
%% into e.g. on/off for display to the user.
%%
%% Ideally cuttlefish should provide some way of converting values back to
%% their user-friendly versions (like what you would see in the config file
%% or pass to riak-admin set) but that may require some more in-depth work...
-spec get_cuttlefish_flags([{string(), cuttlefish_mapping:mapping()}]) -> cuttlefish_flag_list().
get_cuttlefish_flags(KeyMappings) ->
    NormalizeFlag = fun({_, M}) ->
                            case cuttlefish_mapping:datatype(M) of
                                [flag] ->
                                    {flag, on, off};
                                [{flag, TrueVal, FalseVal}] ->
                                    {flag, TrueVal, FalseVal};
                                _ ->
                                    undefined
                            end
                    end,
    lists:map(NormalizeFlag, KeyMappings).

-spec app_config_flags_error() -> err().
app_config_flags_error() ->
    Msg = "Cannot use --all(-a) and --node(-n) at the same time",
    {error, {invalid_flag_combination, Msg}}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

schema_paths_test() ->
    ok = file:write_file("example.schema", <<"thisisnotarealschema">>),
    {ok, Cwd} = file:get_cwd(),
    Schemas = schema_paths([Cwd]),
    ?assertEqual([Cwd++"/example.schema"], Schemas),
    ok = file:delete("example.schema"),
    ?assertEqual([], schema_paths([Cwd])).

set_config_test_() ->
    {setup,
     fun set_config_test_setup/0,
     fun set_config_test_teardown/1,
     [
      fun test_blacklisted_conf/0,
      fun test_set_basic/0,
      fun test_set_bad_flags/0,
      fun test_set_all_flag/0,
      fun test_set_node_flag/0,
      fun test_set_config_callback/0,
      fun test_set_callback_output/0
     ]}.

-define(SET_TEST_SCHEMA_FILE, "test.schema").

set_config_test_setup() ->
    Schema = <<"{mapping, \"test.config\", \"clique.config_test\", [{datatype, integer}]}.">>,
    {ok, Cwd} = file:get_cwd(),

    ?assertEqual(ok, clique_nodes:init()),
    ?assertEqual(true, clique_nodes:register(fun() -> [node()] end)),

    ?assertEqual(ok, file:write_file(?SET_TEST_SCHEMA_FILE, Schema)),
    ?assertEqual(ok, init()),
    ?assertEqual(ok, load_schema([Cwd])).

set_config_test_teardown(_) ->
    _ = ets:delete(?config_table),
    _ = ets:delete(?schema_table),
    _ = ets:delete(?whitelist_table),
    _ = ets:delete(?formatter_table),

    file:delete(?SET_TEST_SCHEMA_FILE),

    clique_nodes:teardown().

test_blacklisted_conf() ->
    true = ets:delete_all_objects(?whitelist_table),
    ?assertEqual({error, {config_not_settable, ["test.config"]}}, set([{"test.config", "42"}], [])).

test_set_basic() ->
    ?assertEqual(ok, whitelist(["test.config"])),

    Result = set([{"test.config", "42"}], []),
    ?assertNotMatch({error, _}, Result),
    ?assertEqual({ok, 42}, application:get_env(clique, config_test)).

test_set_bad_flags() ->
    Result = set([{"test.config", "43"}], [{all, undefined}, {node, node()}]),
    ?assertMatch({error, {invalid_flag_combination, _}}, Result).

test_set_all_flag() ->
    ?assertEqual(ok, whitelist(["test.config"])),
    Result = set([{"test.config", "44"}], [{all, undefined}]),
    ?assertNotMatch({error, _}, Result),
    ?assertEqual({ok, 44}, application:get_env(clique, config_test)).

test_set_node_flag() ->
    ?assertEqual(ok, whitelist(["test.config"])),
    Result = set([{"test.config", "45"}], [{node, node()}]),
    ?assertNotMatch({error, _}, Result),
    ?assertEqual({ok, 45}, application:get_env(clique, config_test)).

test_set_config_callback() ->
    true = ets:delete_all_objects(?config_table),
    Callback = fun(Key, Val) -> 
                       ?assertEqual(["test", "config"], Key),
                       application:set_env(clique, config_test_x10, 10 * list_to_integer(Val)),
                       "Callback called"
               end,
    ?MODULE:register(["test", "config"], Callback),
    set([{"test.config", "47"}], []),
    ?assertEqual({ok, 47}, application:get_env(clique, config_test)),
    ?assertEqual({ok, 470}, application:get_env(clique, config_test_x10)).

test_set_callback_output() ->
    true = ets:delete_all_objects(?config_table),
    Callback = fun(_, _) -> "Done" end,
    ?MODULE:register(["test", "config"], Callback),

    ExpectedText = <<"test.config set to \"48\"\nDone">>,
    %% Slightly fragile way to test this, since we assume the internal representation
    %% for clique text statuses won't change in the future. But this seems better than
    %% any alternative I can think of, since two different iolists representing the
    %% same data may or may not compare equal.
    [{text, OutText}] = set([{"test.config", "48"}], []),
    ?assertEqual(ExpectedText, iolist_to_binary(OutText)),

    ExpectedRow = [{"Node", node()},
                   {"Node Down/Unreachable", false},
                   {"Result", OutText}],
    ExpectedTable = [clique_status:table([ExpectedRow])],
    Result = set([{"test.config", "48"}], [{all, undefined}]),
    ?assertEqual(ExpectedTable, Result).

-endif.
