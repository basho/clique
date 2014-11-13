-module(riak_cli_status).

%% API
-export([parse/3,
         text/1,
         column/2,
         table/1,
         alert/1]).


-include("riak_cli_status_types.hrl").

-spec parse(status(), fun(), Acc0 :: term()) -> term().
parse([], Fun, Acc) ->
    Fun(done, Acc);
%% Alert is currently the only non-leaf element
parse([{alert, Elem} | T], Fun, Acc) ->
    Acc1 = Fun(alert, Acc),
    Acc2 = parse(Elem, Fun, Acc1),
    Acc3 = Fun(alert_done, Acc2),
    parse(T, Fun, Acc3);
%% Leaf elements
parse([Elem | T], Fun, Acc) ->
    Acc1 = Fun(Elem, Acc),
    parse(T, Fun, Acc1).

-spec text(iolist()) -> text().
text(IoList) ->
    {text, IoList}.

%% @doc A column has an attribute and a list of values for that attribute.
-spec column(iolist(), [iolist()]) -> column().
column(Title, Values) ->
    {column, Title, Values}.

%% @doc A table is a zipped list of attribute/val pairs. Each zipped list
%% represents a row and the first element of each pair in the first list is the
%% title of the tables.
-spec table([{iolist(), iolist()}]) -> table().
table(Proplist) ->
   {table, Proplist}.

%% A list of elements
-spec alert([column() | table() | text()]) -> alert().
alert(List) ->
    {alert, List}.
