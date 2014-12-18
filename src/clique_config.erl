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

%% API
-export([init/0,
         register/2,
         show/1,
         set/1,
         whitelist/1,
         describe/1]).

%% Callbacks for rpc calls
-export([get_local_env_status/2,
         get_local_env_vals/2,
         set_local_app_config/1]).

-define(config_table, clique_config).
-define(schema_table, clique_schema).
-define(whitelist_table, clique_whitelist).

-type err() :: {error, term()}.
-type status() :: clique_status:status().
-type proplist() :: [{atom(), term()}].
-type flags() :: [{atom() | char(), term()}].

-type envkey() :: {string(), {atom(), atom()}}.
-type cuttlefish_flag_spec() :: {flag, atom(), atom()}.
-type cuttlefish_flag_list() :: [undefined | cuttlefish_flag_spec()].

%% @doc Register configuration callbacks for a given config key
-spec register([string()], fun()) -> true.
register(Key, Callback) ->
    ets:insert(?config_table, {Key, Callback}).

init() ->
    _ = ets:new(?config_table, [public, named_table]),
    _ = ets:new(?schema_table, [public, named_table]),
    _ = ets:new(?whitelist_table, [public, named_table]),
    SchemaFiles = filelib:wildcard(code:lib_dir() ++ "/*.schema"),
    Schema = cuttlefish_schema:files(SchemaFiles),
    true = ets:insert(?schema_table, {schema, Schema}),
    ok.

%% TODO: This doesn't work for keys with translations.
%% This should almost certainly show the riak.conf value.
%% But, there's not currently any way to reverse cuttlefish
%% translations to get from the application env value back
%% to the value you'd see in the config file or use with
%% the "set" command. We do support flags because those are
%% reversible, but to fully support all possible values we'll
%% have to add reverse translation support to cuttlefish.
-spec show([string()]) -> clique_status:status() | err().
show(KeysAndFlags) ->
    case get_valid_mappings(KeysAndFlags) of
        {error, _}=E ->
            E;
        {[], _Flags} ->
            {error, show_no_args};
        {KeyMappings, Flags0}->
            EnvKeys = get_env_keys(KeyMappings),
            CuttlefishFlags = get_cuttlefish_flags(KeyMappings),
            case clique_parser:validate_flags(config_flags(), Flags0) of
                {error, _}=E ->
                    E;
                Flags ->
                    get_env_status(EnvKeys, CuttlefishFlags, Flags)
            end
    end.

-spec describe([string()]) -> clique_status:status() | err().
describe(KeysAndFlags) ->
    case get_valid_mappings(KeysAndFlags) of
        {error, _}=E ->
            E;
        {[], _Flags} ->
            {error, describe_no_args};
        %% TODO: Do we want to allow any flags? --verbose maybe?
        {KeyMappings, _Flags0} ->
            [begin
                 Doc = cuttlefish_mapping:doc(M),
                 Name = cuttlefish_variable:format(cuttlefish_mapping:variable(M)),
                 clique_status:text(Name ++ ":\n  " ++ string:join(Doc,"\n  ") ++ "\n")
             end || {_, M} <- KeyMappings]
    end.

-spec set([string()]) -> status() | err().
set(ArgsAndFlags) ->
    M1 = clique_parser:parse(ArgsAndFlags),
    M2 = get_config(M1),
    M3 = set_config(M2),
    case run_callback(M3) of
        {error, _}=E ->
            E;
        _ ->
            []
    end.

%% @doc Whitelist settable cuttlefish variables. By default all variables are not settable.
-spec whitelist([string()]) -> ok | {error, {invalid_config_keys, [string()]}}.
whitelist(Keys) ->
    case get_valid_mappings(Keys) of
        {error, _}=E ->
            E;
        {_, _} ->
            _ = [ets:insert(?whitelist_table, {Key}) || Key <- Keys],
            ok
    end.

-spec keys_in_whitelist([string()]) -> true | {error, {config_not_settable, [string()]}}.
keys_in_whitelist(Keys) ->
    Invalid =lists:foldl(fun(K, Acc) ->
                             case ets:lookup(?whitelist_table, K) of
                                 [{_K}] ->
                                     Acc;
                                 [] ->
                                     [K | Acc]
                             end
                         end, [], Keys),
    case Invalid of
        [] -> true;
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
                case {CFlagSpec, Val} of
                    {{flag, TrueVal, _}, true} ->
                        {KeyStr, TrueVal};
                    {{flag, _, FalseVal}, false} ->
                        {KeyStr, FalseVal};
                    _ ->
                        {KeyStr, Val}
                end
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
                    {proplist(), proplist(), flags()} | err().
get_config({error, _}=E) ->
    E;
get_config({[], _Flags}) ->
    {error, set_no_args};
get_config({Args, Flags0}) ->
    [{schema, Schema}] = ets:lookup(?schema_table, schema),
    Conf = [{cuttlefish_variable:tokenize(atom_to_list(K)), V} || {K, V} <- Args],
    case cuttlefish_generator:minimal_map(Schema, Conf) of
        {error, _, Msg} ->
            {error, {invalid_config, Msg}};
        AppConfig ->
            case clique_parser:validate_flags(config_flags(), Flags0) of
                {error, _}=E ->
                    E;
                Flags ->
                    {AppConfig, Conf, Flags}
            end
    end.

-spec set_config(err()) -> err();
      ({proplist(), proplist(), flags()}) -> {proplist(), flags()}| err().
set_config({error, _}=E) ->
    E;
set_config({AppConfig, Args, Flags}) ->
    Keys = [cuttlefish_variable:format(K) || {K, _}  <- Args],
    case keys_in_whitelist(Keys) of
        true ->
            case set_app_config(AppConfig, Flags) of
                ok ->
                    {Args, Flags};
                E ->
                    E
            end;
        {error, _}=E ->
            E
    end.

-spec set_app_config(proplist(), flags()) -> ok | err().
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

-spec set_local_app_config(proplist()) -> ok.
set_local_app_config(AppConfig) ->
    _ = [application:set_env(App, Key, Val) || {App, Settings} <- AppConfig,
                                               {Key, Val} <- Settings],
    ok.

-spec set_remote_app_config(proplist(), node()) -> ok.
set_remote_app_config(AppConfig, Node) ->
    Fun = set_local_app_config,
    case clique_nodes:safe_rpc(Node, ?MODULE, Fun, [AppConfig]) of
        {badrpc, rpc_process_down} ->
            {error, {rpc_process_down, Node}};
        {badrpc, nodedown} ->
            {error, {nodedown, Node}};
        ok ->
            ok
    end.

-spec set_remote_app_config(proplist()) -> ok.
set_remote_app_config(AppConfig) ->
    io:format("Setting config across the cluster~n", []),
    {_, Down} = rpc:multicall(clique_nodes:nodes(),
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
             {typecast, fun clique_typecast:to_node/1},
             {description,
                 "The node to apply the operation on"}]},

     {all, [{shortname, "a"},
            {longname, "all"},
            {description,
                "Apply the operation to all nodes in the cluster"}]}].


-spec get_valid_mappings([string()]) -> err() |
                                        {[{string(), cuttlefish_mapping:mapping()}], flags()}.
get_valid_mappings(KeysAndFlags) ->
    {Keys0, Flags0} = lists:splitwith(fun clique_parser:is_not_flag/1, KeysAndFlags),
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
            case clique_parser:parse_flags(Flags0) of
                {error, _}=E ->
                    E;
                Flags ->
                    {KeyMappings, Flags}
            end
    end.

-spec valid_mappings([cuttlefish_variable:variable()], [tuple()]) -> [tuple()].
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
