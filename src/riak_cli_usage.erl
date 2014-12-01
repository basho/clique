-module(riak_cli_usage).


-define(usage_table, riak_cli_usage).

-type err() :: {error, term()}.

%% API
-export([init/0,
         find/1,
         register/2,
         print/1]).


init() ->
    _ = ets:new(?usage_table, [public, named_table]),
    ok.

%% @doc Register usage for a given command sequence. Lookups are by longest
%% match.
-spec register([string()], iolist()) -> true.
register(Cmd, Usage0) ->
    Usage = ["Usage: ", Usage0],
    ets:insert(?usage_table, {Cmd, Usage}).

-spec print(iolist()) -> ok.
print(Cmd) ->
    Usage = case find(Cmd) of
                {error, Error} ->
                    Error;
                Usage2 ->
                    Usage2
            end,
    io:format("~s", [Usage]).

-spec find(iolist()) -> iolist() | err().
find([]) ->
    {error, "Error: Usage information not found for the given command\n\n"};
find(Cmd) ->
    case ets:lookup(?usage_table, Cmd) of
        [{Cmd, Usage}] ->
            Usage;
        [] ->
            Cmd2 = lists:reverse(tl(lists:reverse(Cmd))),
            find(Cmd2)
    end.

