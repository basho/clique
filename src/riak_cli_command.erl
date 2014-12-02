-module(riak_cli_command).

-define(cmd_table, riak_cli_commands).


%% API
-export([init/0,
         run/1,
         match/1,
         register/4]).

-type err() :: {error, term()}.
-type proplist() :: [{atom(), term()}].

init() ->
    _ = ets:new(?cmd_table, [public, named_table]),
    ok.

%% @doc Register a cli command (i.e.: "riak-admin handoff status")
-spec register([string()], list(), list(), fun()) -> true.
register(Cmd, Keys, Flags, Fun) ->
    ets:insert(?cmd_table, {Cmd, Keys, Flags, Fun}).

-spec run(err()) -> err();
         ({fun(), proplist(), proplist()})-> ok | err().
run({error, _}=E) ->
    riak_cli_error:print(E),
    E;
run({Fun, Args, Flags}) ->
    Fun(Args, Flags).

-spec match([list()])-> {tuple(), list()} | {error, no_matching_spec}.
match(Cmd0) ->
    {Cmd, Args} = split_command(Cmd0),
    case ets:lookup(?cmd_table, Cmd) of
        [Spec] ->
            {Spec, Args};
        [] ->
            {error, {no_matching_spec, Cmd0}}
    end.

-spec split_command([list()]) -> {list(), list()}.
split_command(Cmd0) ->
    lists:splitwith(fun(Str) ->
                        riak_cli_parser:is_not_kv_arg(Str) andalso
                        riak_cli_parser:is_not_flag(Str)
                    end, Cmd0).

