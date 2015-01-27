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
-export([get_local_env_status/2,
         get_local_env_vals/2,
         set_local_app_config/1]).

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
set(Args, Flags) ->
    M1 = get_config({Args, Flags}),
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

-spec get_env_status([envkey()], cuttlefish_flag_list(), flags()) -> status() |
                                                                                         err().
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

-spec get_remote_env_status([envkey()], cuttlefish_flag_list(), node()) ->
    status().
get_remote_env_status(EnvKeys, CuttlefishFlags, Node) ->
    case clique_nodes:safe_rpc(Node, ?MODULE, get_local_env_status,
                               [EnvKeys, CuttlefishFlags]) of
        {badrpc, rpc_process_down} ->
            {error, {rpc_process_down, Node}};
        {badrpc, nodedown} ->
            {error, {nodedown, Node}};
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
                  ([{[string()], string(), status()}]) -> status().
run_callback({error, _}=E) ->
    E;
run_callback({Args, Flags, Status}) ->
    KVFuns = lists:foldl(fun({K, V}, Acc) ->
                             case ets:lookup(?config_table, K) of
                                 [{K, F}] ->
                                     [{K, V, F} | Acc];
                                 [] ->
                                     Acc
                             end
                         end, [], Args),
    _ = [F(K, V, Flags) || {K, V, F} <- KVFuns],
    Status.

-spec get_config(err()) -> err();
                ({args(), flags()}) ->
                    {proplist(), proplist(), flags()} | err().
get_config({error, _}=E) ->
    E;
get_config({[], _Flags}) ->
    {error, set_no_args};
get_config({Args, Flags}) ->
    [{schema, Schema}] = ets:lookup(?schema_table, schema),
    Conf = [{cuttlefish_variable:tokenize(K), V} || {K, V} <- Args],
    case cuttlefish_generator:minimal_map(Schema, Conf) of
        {error, _, Msg} ->
            {error, {invalid_config, Msg}};
        AppConfig ->
            {AppConfig, Conf, Flags}
    end.

-spec set_config(err()) -> err();
      ({proplist(), proplist(), flags()}) -> {proplist(), flags(), status()} | err().
set_config({error, _}=E) ->
    E;
set_config({AppConfig, Args, Flags}) ->
    Keys = [cuttlefish_variable:format(K) || {K, _}  <- Args],
    case check_keys_in_whitelist(Keys) of
        ok ->
            case set_app_config(AppConfig, Flags) of
                {error, _} = E ->
                    E;
                 Status ->
                    {Args, Flags, Status}
            end;
        {error, _}=E ->
            E
    end.

-spec set_app_config(proplist(), flags()) -> status() | err().
set_app_config(AppConfig, []) ->
    {ok, Status} = set_local_app_config(AppConfig),
    Status;
set_app_config(AppConfig, Flags) when length(Flags) =:= 1 ->
    [{Key, Val}] = Flags,
    case Key of
        node -> set_remote_app_config(AppConfig, Val);
        all -> set_remote_app_config(AppConfig)
    end;
set_app_config(_AppConfig, _Flags) ->
    app_config_flags_error().

-spec set_local_app_config(proplist()) -> {ok, status()}.
set_local_app_config(AppConfig) ->
    _ = [application:set_env(App, Key, Val) || {App, Settings} <- AppConfig,
                                               {Key, Val} <- Settings],
    %% We return {ok, Status} instead of just Status so that we can more easily
    %% differentiate a successful return from an RPC error when we call this
    %% remotely.
    {ok, []}.

-spec set_remote_app_config(proplist(), node()) -> status() | err().
set_remote_app_config(AppConfig, Node) ->
    Fun = set_local_app_config,
    case clique_nodes:safe_rpc(Node, ?MODULE, Fun, [AppConfig]) of
        {badrpc, rpc_process_down} ->
            {error, {rpc_process_down, Node}};
        {badrpc, nodedown} ->
            {error, {nodedown, Node}};
        {ok, Status} ->
            Status
    end.

-spec set_remote_app_config(proplist()) -> status().
set_remote_app_config(AppConfig) ->
    %% TODO Convert this io:format to a status? Maybe better to keep it as
    %% an io:format though, since then if the rpc:multicall takes a while,
    %% the user can still see what we're currently trying to do instead of
    %% getting no feedback?
    io:format("Setting config across the cluster~n", []),
    {_, Down} = rpc:multicall(clique_nodes:nodes(),
                              ?MODULE,
                              set_local_app_config,
                              [AppConfig],
                              60000),
    case Down of
        [] ->
            [];
        _ ->
            Alert = io_lib:format("Failed to set config for: ~p~n", [Down]),
            [clique_status:alert([clique_status:text(Alert)])]
    end.

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

-endif.
