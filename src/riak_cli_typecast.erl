-module(riak_cli_typecast).

-export([to_node/1]).

%% This typecast automatically checks if the node is in our cluster,
%% since the atom will have to be registered
-spec to_node(string()) -> node() | {error, bad_node}.
to_node(Str) ->
    try
        list_to_existing_atom(Str)
    catch error:badarg ->
        {error, bad_node}
    end.
