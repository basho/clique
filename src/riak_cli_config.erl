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
-module(riak_cli_config).

%% API
-export([init/0,
         register/2,
         show/1,
         set/1,
         describe/1]).

%% Callbacks for rpc calls
-export([get_local_env_status/2,
         get_local_env_vals/1,
         set_local_app_config/1]).

-define(config_table, riak_cli_config).
-define(schema_table, riak_cli_schema).

-type err() :: {error, term()}.
-type status() :: riak_cli_status:status().
-type proplist() :: [{atom(), term()}].
-type flags() :: [{atom() | char(), term()}].

%% @doc Register configuration callbacks for a given config key
-spec register([string()], fun()) -> true.
register(Key, Callback) ->
    ets:insert(?config_table, {Key, Callback}).

init() ->
    _ = ets:new(?config_table, [public, named_table]),
    _ = ets:new(?schema_table, [public, named_table]),
    SchemaFiles = filelib:wildcard(code:lib_dir() ++ "/*.schema"),
    Schema = cuttlefish_schema:files(SchemaFiles),
    true = ets:insert(?schema_table, {schema, Schema}),
    ok.

%% TODO: This doesn't work for keys with translations.
%% This should almost certainly show the riak.conf value. For now it only works
%% for 1:1 mappings because of we don't save the user value AFAIK.
-spec show([string()]) -> riak_cli_status:status() | err().
show(KeysAndFlags) ->
    case get_valid_mappings(KeysAndFlags) of
        {error, _}=E ->
            E;
        {_Keys, [], _Flags} ->
            {error, config_no_args};
        {Keys, Mappings, Flags0}->
            AppKeyPairs = get_env_keys(Mappings),
            case riak_cli_parser:validate_flags(config_flags(), Flags0) of
                {error, _}=E ->
                    E;
                Flags ->
                    get_env_status(Keys, AppKeyPairs, Flags)
            end
    end.

-spec describe([string()]) -> riak_cli_status:status() | err().
describe(KeysAndFlags) ->
    case get_valid_mappings(KeysAndFlags) of
        {error, _}=E ->
            E;
        {_Keys, [], _Flags} ->
            {error, config_no_args};
        %% TODO: Do we want to allow any flags? --verbose maybe?
        {_Keys, Mappings, _Flags0} ->
            [begin
                 Doc = cuttlefish_mapping:doc(M),
                 Name = cuttlefish_variable:format(cuttlefish_mapping:variable(M)),
                 riak_cli_status:text(Name ++ ":\n  " ++ string:join(Doc,"\n  ") ++ "\n")
             end || M <- Mappings]
    end.

-spec set([string()]) -> status() | err().
set(ArgsAndFlags) ->
    M1 = riak_cli_parser:parse(ArgsAndFlags),
    M2 = get_config(M1),
    M3 = set_config(M2),
    case run_callback(M3) of
        {error, _}=E ->
            E;
        _ ->
            []
    end.

-spec get_env_status([string()], [{atom(), atom()}], flags()) -> status() |
                                                                 err().
get_env_status(Keys, AppKeyPairs, []) ->
    get_local_env_status(Keys, AppKeyPairs);
get_env_status(Keys, AppKeyPairs, Flags) when length(Flags) =:= 1 ->
    [{Key, Val}] = Flags,
    case Key of
        node -> get_remote_env_status(Keys, AppKeyPairs, Val);
        all -> get_remote_env_status(Keys, AppKeyPairs)
    end;
get_env_status(_Keys, _AppKeyPairs, _Flags) ->
    app_config_flags_error().

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
    case riak_cli_nodes:safe_rpc(Node, ?MODULE, get_local_env_status, [Keys, AppKeyPairs]) of
        {badrpc, rpc_process_down} ->
            io:format("Error: Node ~p Down~n", [Node]),
            [];
        Status ->
            Status
    end.

-spec get_remote_env_status([string()], [{atom(), atom()}]) -> status().
get_remote_env_status(Keys0, AppKeyPairs) ->
    Nodes = riak_cli_nodes:nodes(),
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
    {error, config_no_args};
get_config({Args, Flags0}) ->
    [{schema, Schema}] = ets:lookup(?schema_table, schema),
    Conf = [{cuttlefish_variable:tokenize(atom_to_list(K)), V} || {K, V} <- Args],
    case cuttlefish_generator:minimal_map(Schema, Conf) of
        {error, _, Msg} ->
            {error, {invalid_config, Msg}};
        AppConfig ->
            case riak_cli_parser:validate_flags(config_flags(), Flags0) of
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
    case set_app_config(AppConfig, Flags) of
        ok ->
            {Args, Flags};
        E ->
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
    case riak_cli_nodes:safe_rpc(Node, ?MODULE, Fun, [AppConfig]) of
        {badrpc, _} ->
            ok;
        ok ->
            ok
    end.

-spec set_remote_app_config(proplist()) -> ok.
set_remote_app_config(AppConfig) ->
    io:format("Setting config across the cluster~n", []),
    {_, Down} = rpc:multicall(riak_cli_nodes:nodes(),
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
             {typecast, fun riak_cli_typecast:to_node/1},
             {description,
                 "The node to apply the operation on"}]},

     {all, [{shortname, "a"},
            {longname, "all"},
            {description,
                "Apply the operation to all nodes in the cluster"}]}].


-spec get_valid_mappings([string()]) -> err() | {[string()], list(tuple()), flags()}.
get_valid_mappings(KeysAndFlags) ->
    {Keys0, Flags0} = lists:splitwith(fun riak_cli_parser:is_not_flag/1, KeysAndFlags),
    Keys = [cuttlefish_variable:tokenize(K) || K <- Keys0],
    [{schema, Schema}] = ets:lookup(?schema_table, schema),
    {_Translations, Mappings0, _Validators} = Schema,
    Mappings = valid_mappings(Keys, Mappings0),
    case length(Mappings) =:= length(Keys) of
        false ->
            Invalid = invalid_keys(Keys, Mappings),
            {error, {invalid_config_keys, Invalid}};
        true ->
            case riak_cli_parser:parse_flags(Flags0) of
                {error, _}=E ->
                    E;
                Flags ->
                    {Keys0, Mappings, Flags}
            end
    end.

-spec valid_mappings([cuttlefish_variable:variable()], [tuple()]) -> [tuple()].
valid_mappings(Keys, Mappings) ->
    lists:filter(fun(Mapping) ->
                     Key = element(2, Mapping),
                     lists:member(Key, Keys)
                 end, Mappings).

-spec invalid_keys([cuttlefish_variable:variable()],
                   [cuttlefish_mapping:mapping()]) -> [string()].
invalid_keys(Keys, Mappings) ->
    Invalid = lists:filter(fun(Key) ->
                               not lists:keymember(Key, 2, Mappings)
                           end, Keys),
   [cuttlefish_variable:format(I)++" " || I <- Invalid].

-spec get_env_keys(list(tuple())) -> [{atom(), atom()}].
get_env_keys(Mappings) ->
    EnvStrs = [element(3, M) || M <- Mappings],
    AppAndKeys = [string:tokens(S, ".") || S <- EnvStrs],
    [{list_to_atom(App), list_to_atom(Key)} || [App, Key] <-  AppAndKeys].

-spec app_config_flags_error() -> err().
app_config_flags_error() ->
    Msg = "Cannot use --all(-a) and --node(-n) at the same time",
    {error, {invalid_flag_combination, Msg}}.

