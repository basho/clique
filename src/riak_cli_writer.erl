-module(riak_cli_writer).

%% @doc This module provides callback functions to the status parsing code in
%% riak_cli_status:parse/3. It specifically formats the output for the console
%% and handles an opaque context passed back during parsing.

%% API
-export([write/1]).

-include("riak_cli_status_types.hrl").

-record(context, {alert_set=false :: boolean(),
                  output="" :: iolist()}).

-spec write(status()) -> iolist().
write(Status) ->
    Ctx = riak_cli_status:parse(Status, fun write_status/2, #context{}),
    Ctx#context.output.

%% @doc Write status information in console format.
-spec write_status(elem(), #context{}) -> #context{}.
write_status(alert, Ctx=#context{alert_set=false}) ->
    Ctx#context{alert_set=true};
write_status(alert, Ctx) ->
    %% TODO: Should we just return an error instead?
    throw({error, nested_alert, Ctx});
write_status(alert_done, Ctx) ->
    Ctx#context{alert_set=false};
write_status({column, Title, Data}, Ctx=#context{output=Output}) ->
    Ctx#context{output=Output++write_column(Title, Data)};
write_status({text, Text}, Ctx=#context{output=Output}) ->
    Ctx#context{output=Output++Text++"\n"};
write_status({table, Rows}, Ctx=#context{output=Output}) ->
    Ctx#context{output=Output++write_table(Rows)};
write_status(done, Ctx) ->
    Ctx.

-spec write_table([{iolist(), iolist()}]) -> iolist().
write_table([]) ->
    "";
write_table(Rows) ->
    Schema = [Name || {Name, _Val} <- hd(Rows)],
    Table = riak_cli_table:autosize_create_table(Schema, Rows),
    io_lib:format("~ts~n", [Table]).

%% @doc Write a column on a single line.
write_column(Title, Items) when is_atom(Title) ->
    write_column(atom_to_list(Title), Items);
%% Assume all items are of same type
write_column(Title, Items) when is_atom(hd(Items)) ->
    Items2 = [atom_to_list(Item) || Item <- Items],
    write_column(Title, Items2);
write_column(Title, Items) ->
    %% Todo: add bold/color for Title when supported
    lists:foldl(fun(Item, Acc) ->
                    Acc++" "++Item
                end, Title++":", Items) ++ "\n".

