-module(riak_cli_nodes).

-export([init/0,
         safe_rpc/4,
         nodes/0,
         register/1]).

-define(nodes_table, riak_cli_nodes).

init() ->
    _ = ets:new(?nodes_table, [public, named_table]),
    ok.

-spec register(fun()) -> true.
register(Fun) ->
    ets:insert(?nodes_table, {nodes_fun, Fun}).

-spec nodes() -> [node()].
nodes() ->
    [{nodes_fun, Fun}] = ets:lookup(?nodes_table, nodes_fun),
    Fun().

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

