-module(riak_cli_status).

%% API
-export([parse/3,
         text/1,
         column/2,
         table/1,
         alert/1,
         is_status/1]).


-include("riak_cli_status_types.hrl").

-export_type([status/0]).

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

%% @doc Is the given value a status type?
-spec is_status(any()) -> boolean().
is_status({text, _}) ->
    true;
is_status({column, _, _}) ->
    true;
is_status({table, _, _}) ->
    true;
is_status({alert, _}) ->
    true;
is_status(_) ->
    false.

-spec text(iolist()) -> text().
text(IoList) ->
    {text, IoList}.

%% @doc A column has an attribute and a list of values for that attribute.
-spec column(iolist(), [iolist()]) -> column().
column(Title, Values) ->
    {column, Title, Values}.

%% @doc A table is constructed from a list of proplists. Each proplist
%% represents a row in the table. The keys in the first row represent
%% column headers; each following row (proplist) must contain the same
%% number of tagged tuples but the keys are ignored.
-spec table([[{atom() | string(), term()}]]) -> table().
table(Proplists) ->
   {table, Proplists}.

%% A list of elements
-spec alert([column() | table() | text()]) -> alert().
alert(List) ->
    {alert, List}.
