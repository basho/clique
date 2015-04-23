%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013, 2014 Basho Technologies, Inc.
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

-module(clique_table).

%% API
-export([print/2, print/3,
         create_table/2,
         autosize_create_table/2, autosize_create_table/3]).

-include("clique_status_types.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(MAX_LINE_LEN, 100).
-define(else, true).
-define(MINWIDTH(W),
        if W =< 0 ->
                1;
           ?else ->
                W
        end).

-spec print(list(), list()) -> ok.
print(_Spec, []) ->
    ok;
%% Explict sizes were not given. This is called using the new status types.
print(Schema, Rows) when is_list(hd(Schema)) ->
    Table = autosize_create_table(Schema, Rows),
    io:format("~n~ts~n", [Table]);
print(Spec, Rows) ->
    Table = create_table(Spec, Rows),
    io:format("~n~ts~n", [Table]).

-spec print(list(), list(), list()) -> ok.
print(_Hdr, _Spec, []) ->
    ok;
print(Header, Spec, Rows) ->
    Table = create_table(Spec, Rows),
    io:format("~ts~n~n~ts~n", [Header, Table]).

-spec autosize_create_table([any()], [[any()]]) -> iolist().
autosize_create_table(Schema, Rows) ->
    autosize_create_table(Schema, Rows, []).

%% Currently the only constraint supported in the proplist is
%% `fixed_width' with a list of columns that *must not* be shrunk
%% (e.g., integer values). First column is 0.
-spec autosize_create_table([any()], [[any()]], [tuple()]) -> iolist().
autosize_create_table(Schema, Rows, Constraints) ->
    BorderSize = 1 + length(hd(Rows)),
    MaxLineLen = case io:columns() of
                     %% Leaving an extra space seems to work better
                     {ok, N} -> N - 1;
                     {error, enotsup} -> ?MAX_LINE_LEN
                 end,
    Sizes = get_field_widths(MaxLineLen - BorderSize, [Schema | Rows],
                             proplists:get_value(fixed_width, Constraints, [])),
    Spec = lists:zip(Schema, Sizes),
    create_table(Spec, Rows, MaxLineLen, []).

-spec create_table(list(), list()) -> iolist().
create_table(Spec, Rows) ->
    Lengths = get_row_length(Spec, Rows),
    Length = lists:sum(Lengths)+2,
    AdjustedSpec = [{Field, NewLength} || {{Field, _DefaultLength}, NewLength}
                                          <- lists:zip(Spec, Lengths)],
    create_table(AdjustedSpec, Rows, Length, []).

-spec create_table(list(), list(), non_neg_integer(), iolist()) -> iolist().
create_table(Spec, Rows, Length, []) ->
    FirstThreeRows = [vertical_border(Spec), titles(Spec),
                      vertical_border(Spec)],
    create_table(Spec, Rows, Length, FirstThreeRows);
create_table(_Spec, [], _Length, IoList) when length(IoList) == 3 ->
    %% table had no rows, no final row needed
    lists:reverse(IoList);
create_table(Spec, [], _Length, IoList) ->
    BottomBorder = vertical_border(Spec),
    %% There are no more rows to print so return the table
    lists:reverse([BottomBorder | IoList]);
create_table(Spec, [Row | Rows], Length, IoList) ->
    create_table(Spec, Rows, Length, [row(Spec, Row) | IoList]).

%% Measure and shrink table width as necessary to fit the console
-spec get_field_widths(pos_integer(), [term()], [non_neg_integer()]) -> [non_neg_integer()].
get_field_widths(MaxLineLen, Rows, Unshrinkable) ->
    Widths = max_widths(Rows),
    fit_widths_to_terminal(MaxLineLen, Widths, Unshrinkable).

fit_widths_to_terminal(MaxWidth, Widths, Unshrinkable) ->
    Sum = lists:sum(Widths),
    Weights = calculate_field_weights(Sum, Widths, Unshrinkable),
    MustRemove = Sum - MaxWidth,
    calculate_new_widths(MaxWidth, MustRemove, Widths, Weights).

%% Determine field weighting as proportion of total width of the
%% table. Fields which were flagged as unshrinkable will be given a
%% weight of 0.
-spec calculate_field_weights(pos_integer(), list(pos_integer()),
                              list(non_neg_integer())) ->
                                     list(number()).
calculate_field_weights(Sum, Widths, []) ->
    %% If no fields are constrained as unshrinkable, simply divide
    %% each width by the sum of all widths for our proportions
    lists:map(fun(X) -> X / Sum end, Widths);
calculate_field_weights(_Sum, Widths, Unshrinkable) ->
    TaggedWidths = flag_unshrinkable_widths(Widths, Unshrinkable),
    ShrinkableWidth = lists:sum(lists:filter(fun({_X, noshrink}) -> false;
                                                (_X) -> true end,
                                             TaggedWidths)),
    lists:map(fun({_X, noshrink}) -> 0;
                 (X) -> X / ShrinkableWidth end,
              TaggedWidths).

%% Takes a list of column widths and a list of (zero-based) index
%% values of the columns that must not shrink. Returns a mixed list of
%% widths and `noshrink' tuples.
flag_unshrinkable_widths(Widths, NoShrink) ->
    {_, NewWidths} =
        lists:foldl(fun(X, {Idx, Mapped}) ->
                            case lists:member(Idx, NoShrink) of
                                true ->
                                    {Idx + 1, [{X, noshrink}|Mapped]};
                                false ->
                                    {Idx + 1, [X|Mapped]}
                            end
                    end, {0, []}, Widths),
    lists:reverse(NewWidths).

%% Calculate the proportional weight for each column for shrinking.
%% Zip the results into a `{Width, Weight, Index}' tuple list.
column_zip(Widths, Weights, ToNarrow) ->
    column_zip(Widths, Weights, ToNarrow, 0, []).

column_zip([], [], _ToNarrow, _Index, Accum) ->
    lists:reverse(Accum);
column_zip([Width|Widths], [Weight|Weights], ToNarrow, Index, Accum) ->
    NewWidth = ?MINWIDTH(Width - round(ToNarrow * Weight)),
    column_zip(Widths, Weights, ToNarrow, Index+1,
               [{NewWidth, Weight, Index}] ++ Accum).

%% Given the widths based on data to be displayed, return widths
%% necessary to narrow the table to fit the console.
calculate_new_widths(_Max, ToNarrow, Widths, _Weights) when ToNarrow =< 0 ->
    %% Console is wide enough, no need to narrow
    Widths;
calculate_new_widths(MaxWidth, ToNarrow, Widths, Weights) ->
    fix_rounding(MaxWidth, column_zip(Widths, Weights, ToNarrow)).

%% Rounding may introduce an error. If so, remove the requisite number
%% of spaces from the widest field
fix_rounding(Target, Cols) ->
    Widths = lists:map(fun({Width, _Weight, _Idx}) -> Width end,
                       Cols),
    SumWidths = lists:sum(Widths),
    shrink_widest(Target, SumWidths, Widths, Cols).

%% Determine whether our target table width is wider than the terminal
%% due to any rounding error and find columns eligible to be shrunk.
shrink_widest(Target, Current, Widths, _Cols) when Target =< Current ->
    Widths;
shrink_widest(Target, Current, Widths, Cols) ->
    Gap = Current - Target,
    NonZeroWeighted = lists:dropwhile(fun({_Width, 0, _Idx}) -> true;
                                         (_) -> false end,
                                      Cols),
    shrink_widest_weighted(Gap, NonZeroWeighted, Widths).

%% Take the widest column with a non-zero weight and reduce it by the
%% amount necessary to compensate for any rounding error.
shrink_widest_weighted(_Gap, [], Widths) ->
    Widths; %% All columns constrained to fixed widths, nothing we can do
shrink_widest_weighted(Gap, Cols, Widths) ->
    SortedCols = lists:sort(
                   fun({WidthA, _WeightA, _IdxA}, {WidthB, _WeightB, _IdxB}) ->
                           WidthA > WidthB
                   end, Cols),
    {OldWidth, _Weight, Idx} = hd(SortedCols),
    NewWidth = ?MINWIDTH(OldWidth - Gap),
    replace_list_element(Idx, NewWidth, Widths).

%% Replace the item at `Index' in `List' with `Element'.
%% Zero-based indexing.
-spec replace_list_element(non_neg_integer(), term(), list()) -> list().
replace_list_element(Index, Element, List) ->
    {Prefix, Suffix} = lists:split(Index, List),
    Prefix ++ [Element] ++ tl(Suffix).

get_row_length(Spec, Rows) ->
    Res = lists:foldl(fun({_Name, MinSize}, Total) ->
                        Longest = find_longest_field(Rows, length(Total)+1),
                        Size = erlang:max(MinSize, Longest),
                        [Size | Total]
                end, [], Spec),
    lists:reverse(Res).

-spec find_longest_field(list(), pos_integer()) -> non_neg_integer().
find_longest_field(Rows, ColumnNo) ->
        lists:foldl(fun(Row, Longest) ->
                        erlang:max(Longest,
                                  field_length(lists:nth(ColumnNo,Row)))
                    end, 0, Rows).

-spec max_widths([term()]) -> list(pos_integer()).
max_widths([Row]) ->
    field_lengths(Row);
max_widths([Row1 | Rest]) ->
    Row1Lengths = field_lengths(Row1),
    {ok, F} = file:open("/tmp/fields", [write]),
    io:format(F, "RowLengths = ~p, Row1~p~n", [Row1Lengths, Row1]),
    lists:foldl(fun(Row, Acc) ->
        io:format(F, "Row~p~n", [Row]),
                    Lengths = field_lengths(Row),
        io:format(F, "Lengths = ~p, Row~p~n", [Lengths, Row]),
                    [max(A, B) || {A, B} <- lists:zip(Lengths, Acc)]
                end, Row1Lengths, Rest).

-spec row(list(), list(string())) -> iolist().
row(Spec, Row0) ->
    %% handle multiline fields
    Rows = expand_row(Row0),
    [
     [ $| | lists:reverse(
              ["\n" | lists:foldl(fun({{_, Size}, Str}, Acc) ->
                                          [align(Str, Size) | Acc]
                                  end, [], lists:zip(Spec, Row))])] || Row <- Rows].

-spec titles(list()) -> iolist().
titles(Spec) ->
    [ $| | lists:reverse(
        ["\n" | lists:foldl(fun({Title, Size}, TitleRow) ->
                               [align(Title, Size) | TitleRow]
                            end, [], Spec)])].

-spec align(string(), non_neg_integer()) -> iolist().
align(undefined, Size) ->
    align("", Size);
align(Str, Size) when is_integer(Str) ->
    align(integer_to_list(Str), Size);
align(Str, Size) when is_binary(Str) ->
    align(unicode:characters_to_list(Str, utf8), Size);
align(Str, Size) when is_atom(Str) ->
    align(atom_to_list(Str), Size);
align(Str, Size) when is_list(Str), length(Str) >= Size ->
    Truncated = lists:sublist(Str, Size),
    Truncated ++ "|";
align(Str, Size) when is_list(Str) ->
    string:centre(Str, Size) ++ "|";
align(Term, Size) ->
    Str = lists:flatten(io_lib:format("~p", [Term])),
    align(Str, Size).

-spec vertical_border(list(tuple())) -> string().
vertical_border(Spec) ->
    lists:reverse([$\n, [[char_seq(Length, $-), $+] ||
                             {_Name, Length} <- Spec], $+]).

-spec char_seq(non_neg_integer(), char()) -> string().
char_seq(Length, Char) ->
    [Char || _ <- lists:seq(1, Length)].

field_lengths(Row) ->
    [field_length(Field) || Field <- Row].

field_length(Field) when is_atom(Field) ->
    field_length(atom_to_list(Field));
field_length(Field) when is_binary(Field) ->
    field_length(unicode:characters_to_list(Field, utf8));
field_length(Field) when is_list(Field) ->
    Lines = string:tokens(lists:flatten(Field), "\n"),
    lists:foldl(fun(Line, Longest) ->
                        erlang:max(Longest,
                                   length(Line))
                end, 0, Lines);
field_length(Field) ->
    field_length(io_lib:format("~p", [Field])).

expand_field(Field) when is_atom(Field) ->
    expand_field(atom_to_list(Field));
expand_field(Field) when is_binary(Field) ->
    expand_field(unicode:characters_to_list(Field, utf8));
expand_field(Field) when is_list(Field) ->
    string:tokens(lists:flatten(Field), "\n");
expand_field(Field) ->
    expand_field(io_lib:format("~p", [Field])).

expand_row(Row) ->
    {ExpandedRow, MaxHeight} = lists:foldl(fun(Field, {Fields, Max}) ->
                                                    EF = expand_field(Field),
                                                    {[EF|Fields], erlang:max(Max, length(EF))}
                                            end, {[], 0}, lists:reverse(Row)),
    PaddedRow = [pad_field(Field, MaxHeight) || Field <- ExpandedRow],
    [ [ lists:nth(N, Field) || Field <- PaddedRow]
      || N <- lists:seq(1, MaxHeight)].

pad_field(Field, MaxHeight) when length(Field) < MaxHeight ->
    Field ++ ["" || _ <- lists:seq(1, MaxHeight - length(Field))];
pad_field(Field, _MaxHeight) ->
    Field.

-ifdef(TEST).
-include("clique_test_group_leader.hrl").

table_size_test_() ->
    [
        {"try a table with an empty row", fun test_empty_row/0},
        {"try a table with a row less than the size of screen", fun test_short_row/0},
        {"try a table with a row greater than the size of the screen", fun test_long_row/0},
        {"try a table with a row with mixed values", fun test_mixed_row/0},
        {"try a table with a row equal to the size of the screen", fun test_equal_row/0},
        {"try a table with a stupidly long rows", fun test_bad_rows/0}
    ].

test_empty_row() ->
    ?TEST_GROUP_LEADER(begin
        ?assertError(function_clause, autosize_create_table([<<"One Column">>], [[]]))
    end).

test_short_row() ->
    ?TEST_GROUP_LEADER(begin
        Result = autosize_create_table([<<"One Column">>], [["A single row"]]),
        ?assertEqual(Result, [[43,[["------------",43]],10],
            [124," One Column |","\n"],
            [43,[["------------",43]],10],
            [[124,"A single row|","\n"]],
            [43,[["------------",43]],10]])
    end).

test_long_row() ->
    ?TEST_GROUP_LEADER(begin
        clique_test_group_leader:set_size(group_leader(), 20, 10),
        Result = autosize_create_table([<<"One Column">>],
                                      [[lists:flatten(lists:duplicate(?MAX_LINE_LEN*2,"x"))]]),
        ?assertEqual(Result, [[43,[["-----------------",43]],10],
            [124,"   One Column    |","\n"],
            [43,[["-----------------",43]],10],
            [[124,"xxxxxxxxxxxxxxxxx|","\n"]],
            [43,[["-----------------",43]],10]])
    end).

test_mixed_row() ->
    ?TEST_GROUP_LEADER(begin
        clique_test_group_leader:set_size(group_leader(), 20, 10),
        Result = autosize_create_table([<<"One Column">>],
            [[["SinkNode1LeaderRejoin",32,61,32,"undefined",32,105,115,32,110,111,116,
                32,101,113,117,97,108,32,116,111,32,101,120,112,101,99,116,101,100,32,
                118,97,108,117,101,32,"'dev4@127.0.0.1'",32,97,116,32,108,105,110,101,
                32,"990"]]]),
        ?assertEqual(Result,[[43,[["-----------------",43]],10],
            [124,"   One Column    |","\n"],
            [43,[["-----------------",43]],10],
            [[124,"SinkNode1LeaderRe|","\n"]],
            [43,[["-----------------",43]],10]])
    end).

test_equal_row() ->
    ?TEST_GROUP_LEADER(begin
        clique_test_group_leader:set_size(group_leader(), 20, 10),
        Result = autosize_create_table([<<"One Column">>],
           [[lists:flatten(lists:duplicate(?MAX_LINE_LEN,"x"))]]),
        ?assertEqual(Result, [[43,[["-----------------",43]],10],
           [124,"   One Column    |","\n"],
           [43,[["-----------------",43]],10],
           [[124,"xxxxxxxxxxxxxxxxx|","\n"]],
           [43,[["-----------------",43]],10]])
    end).

test_bad_rows() ->
    ?TEST_GROUP_LEADER(begin
        clique_test_group_leader:set_size(group_leader(), 120, 10),
        {ok, F} = file:open("/tmp/result", [write]),
        io:format(F, "before~n", []),
        Result = autosize_create_table([<<"Test">>, <<"Result">>, <<"Reason">>, <<"Test Duration">>],
           [["replication2_pg:test_12_pg_mode_repl_mixed_ssl","pass","N/A",<<"0h 5m 16.929610s">>],
           ["replication2_pg:test_12_pg_mode_repl_mixed","pass","N/A",<<"0h 3m 33.794586s">>],
           ["replication2_pg:test_12_pg_mode_repl12_ssl","pass","N/A",<<"0h 5m 11.753247s">>],
           ["replication2_pg:test_12_pg_mode_repl12","pass","N/A",<<"0h 3m 30.530697s">>],
           ["replication2","pass","N/A",<<"0h 6m 49.618059s">>],
           ["replication","pass","N/A",<<"0h 2m 46.307434s">>],
           ["repl_fs_stat_caching","pass","N/A",<<"0h 3m 42.619749s">>],
           ["pb_security","pass","N/A",<<"0h 1m 13.116427s">>],
           ["partition_repair",fail,[85,110,107,110,111,119,110,32,101,114,114,111,114,32,101,110,99,111,117,110,116,101,114,101,100,58,32,[123,["timeout",44,10,[32,32,[32,["   ",32,32,32],"   ",32,32,32],32,["   ",32,32,32],"   ",32,32,32],[123,["gen_server",44,"call",44,10,[32,32,32,[32,["   ",32,32,32],"   ",32,32,32],32,["   ",32,32,32],"   ",32,32,32],[91,[[123,["riak_search_vnode_master",44,"'dev3@127.0.0.1'"],125],44,10,[32,32,32,32,[32,["   ",32,32,32],"   ",32,32,32],32,["   ",32,32,32],"   ",32,32,32],[123,["riak_vnode_req_v1",44,10,[32,32,32,32,32,[32,["   ",32,32,32],"   ",32,32,32],32,["   ",32,32,32],"   ",32,32,32],"182687704666362864775460604089535377456991567872",44,10,[32,32,32,32,32,[32,["   ",32,32,32],"   ",32,32,32],32,["   ",32,32,32],"   ",32,32,32],[123,["server",44,"undefined",44,"undefined"],125],44,10,[32,32,32,32,32,[32,["   ",32,32,32],"   ",32,32,32],32,["   ",32,32,32],"   ",32,32,32],[123,["riak_core_fold_req_v1",44,10,[32,32,32,32,32,32,[32,["   ",32,32,32],"   ",32,32,32],32,["   ",32,32,32],"   ",32,32,32],"#Fun<partition_repair.37.105725082>",44,10,[32,32,32,32,32,32,[32,["   ",32,32,32],"   ",32,32,32],32,["   ",32,32,32],"   ",32,32,32],[123,["dict",44,"0",44,"16",44,"16",44,"8",44,"80",44,"48",44,10,[32,32,32,32,32,32,32,[32,["   ",32,32,32],"   ",32,32,32],32,["   ",32,32,32],"   ",32,32,32],[123,["[]",44,"[]",44,"[]",44,"[]",44,"[]",44,"[]",44,"[]",44,"[]",44,"[]",44,"[]",44,"[]",44,"[]",44,"[]",44,"[]",44,10,[32,32,32,32,32,32,32,32,[32,["   ",32,32,32],"   ",32,32,32],32,["   ",32,32,32],"   ",32,32,32],"[]",44,"[]"],125],44,10,[32,32,32,32,32,32,32,[32,["   ",32,32,32],"   ",32,32,32],32,["   ",32,32,32],"   ",32,32,32],[123,[[123,["[]",44,"[]",44,"[]",44,"[]",44,"[]",44,"[]",44,"[]",44,"[]",44,"[]",44,"[]",44,"[]",44,"[]",44,"[]",44,"[]",44,10,[32,32,32,32,32,32,32,32,32,[32,["   ",32,32,32],"   ",32,32,32],32,["   ",32,32,32],"   ",32,32,32],"[]",44,"[]"],125]],125]],125]],125]],125],44,10,[32,32,32,32,[32,["   ",32,32,32],"   ",32,32,32],32,["   ",32,32,32],"   ",32,32,32],"600000"],93]],125]],125]],<<"0h 10m 59.989058s">>],
           ["loaded_upgrade","pass","N/A",<<"0h 18m 16.648265s">>],
           ["loaded_upgrade","pass","N/A",<<"0h 18m 19.476296s">>],
           ["jmx_verify",fail,[85,110,107,110,111,119,110,32,101,114,114,111,114,32,101,110,99,111,117,110,116,101,114,101,100,58,32,[123,["case_clause",44,10,["  ","  ",32,[32,["   ",32,32,32],"   ",32,32,32],32,["   ",32,32,32],"   ",32,32,32],[60,60,"\"Exception in thread \\\"main\\\" java.io.IOException: Failed to retrieve RMIServer stub: javax.naming.ServiceUnavailableException [Root exception is java.rmi.ConnectException: Connection refused to host: 127.0.0.1; nested exception is: \\n\\tjava.net.ConnectException: Connection refused]\\n\\tat javax.management.remote.rmi.RMIConnector.connect(RMIConnector.java:369)\\n\\tat javax.management.remote.JMXConnectorFactory.connect(JMXConnectorFactory.java:268)\\n\\tat com.basho.riak.jmx.Dump.main(Dump.java:40)\\nCaused by: javax.naming.ServiceUnavailableException [Root exception is java.rmi.ConnectException: Connection refused to host: 127.0.0.1; nested exception is: \\n\\tjava.net.ConnectException: Connection refused]\\n\\tat com.sun.jndi.rmi.registry.RegistryContext.lookup(RegistryContext.java:118)\\n\\tat com.sun.jndi.toolkit.url.GenericURLContext.lookup(GenericURLContext.java:203)\\n\\tat javax.naming.InitialContext.lookup(InitialContext.java:411)\\n\\tat javax.management.remote.rmi.RMIConnector.findRMIServerJNDI(RMIConnector.java:1929)\\n\\tat javax.management.remote.rmi.RMIConnector.findRMIServer(RMIConnector.java:1896)\\n\\tat javax.management.remote.rmi.RMIConnector.connect(RMIConnector.java:286)\\n\\t... 2 more\\nCaused by: java.rmi.ConnectException: Connection refused to host: 127.0.0.1; nested exception is: \\n\\tjava.net.ConnectException: Connection refused\\n\\tat sun.rmi.transport.tcp.TCPEndpoint.newSocket(TCPEndpoint.java:619)\\n\\tat sun.rmi.transport.tcp.TCPChannel.createConnection(TCPChannel.java:216)\\n\\tat sun.rmi.transport.tcp.TCPChannel.newConnection(TCPChannel.java:202)\\n\\tat sun.rmi.server.UnicastRef.newCall(UnicastRef.java:341)\\n\\tat sun.rmi.registry.RegistryImpl_Stub.lookup(Unknown Source)\\n\\tat com.sun.jndi.rmi.registry.RegistryContext.lookup(RegistryContext.java:114)\\n\\t... 7 more\\nCaused by: java.net.ConnectException: Connection refused\\n\\tat java.net.PlainSocketImpl.socketConnect(Native Method)\\n\\tat java.net.AbstractPlainSocketImpl.doConnect(AbstractPlainSocketImpl.java:339)\\n\\tat java.net.AbstractPlainSocketImpl.connectToAddress(AbstractPlainSocketImpl.java:200)\\n\\tat java.net.AbstractPlainSocketImpl.connect(AbstractPlainSocketImpl.java:182)\\n\\tat java.net.SocksSocketImpl.connect(SocksSocketImpl.java:392)\\n\\tat java.net.Socket.connect(Socket.java:579)\\n\\tat java.net.Socket.connect(Socket.java:528)\\n\\tat java.net.Socket.<init>(Socket.java:425)\\n\\tat java.net.Socket.<init>(Socket.java:208)\\n\\tat sun.rmi.transport.proxy.RMIDirectSocketFactory.createSocket(RMIDirectSocketFactory.java:40)\\n\\tat sun.rmi.transport.proxy.RMIMasterSocketFactory.createSocket(RMIMasterSocketFactory.java:147)\\n\\tat sun.rmi.transport.tcp.TCPEndpoint.newSocket(TCPEndpoint.java:613)\\n\\t... 12 more\"",62,62]],125]],<<"0h 5m 49.15820s">>],
           ["gh_riak_core_155","pass","N/A",<<"0h 0m 42.22329s">>],
           ["ensemble_interleave","pass","N/A",<<"0h 3m 20.995938s">>],
           ["ensemble_byzantine","pass","N/A",<<"0h 5m 47.976967s">>],
           ["basic_command_line","pass","N/A",<<"0h 0m 44.753746s">>],
           ["verify_staged_clustering","pass","N/A",<<"0h 5m 2.903191s">>],
           ["verify_search","pass","N/A",<<"0h 0m 53.932783s">>],
           ["verify_riak_stats",fail,["{ MissingStatsKeys , AdditionalStatsKeys }",32,61,32,[123,[[91,[[60,60,"\"search_index_latency_mean\"",62,62],44,10,[32,32,32,[[32,[32,"  ",32,32],32,"  ",32,32],32,[32,"  ",32,32],32,"  ",32,32],[32,[32,"  ",32,32],32,"  ",32,32],32,[32,"  ",32,32],32,"  ",32,32],[60,60,"\"search_index_throughput_one\"",62,62],44,10,[32,32,32,[[32,[32,"  ",32,32],32,"  ",32,32],32,[32,"  ",32,32],32,"  ",32,32],[32,[32,"  ",32,32],32,"  ",32,32],32,[32,"  ",32,32],32,"  ",32,32],[60,60,"\"search_query_latency_mean\"",62,62],44,10,[32,32,32,[[32,[32,"  ",32,32],32,"  ",32,32],32,[32,"  ",32,32],32,"  ",32,32],[32,[32,"  ",32,32],32,"  ",32,32],32,[32,"  ",32,32],32,"  ",32,32],[60,60,"\"write_once_merge\"",62,62],44,10,[32,32,32,[[32,[32,"  ",32,32],32,"  ",32,32],32,[32,"  ",32,32],32,"  ",32,32],[32,[32,"  ",32,32],32,"  ",32,32],32,[32,"  ",32,32],32,"  ",32,32],[60,60,"\"write_once_put_objsize_100\"",62,62],44,10,[32,32,32,[[32,[32,"  ",32,32],32,"  ",32,32],32,[32,"  ",32,32],32,"  ",32,32],[32,[32,"  ",32,32],32,"  ",32,32],32,[32,"  ",32,32],32,"  ",32,32],[60,60,"\"write_once_put_objsize_95\"",62,62],44,10,[32,32,32,[[32,[32,"  ",32,32],32,"  ",32,32],32,[32,"  ",32,32],32,"  ",32,32],[32,[32,"  ",32,32],32,"  ",32,32],32,[32,"  ",32,32],32,"  ",32,32],[60,60,"\"write_once_put_objsize_99\"",62,62],44,10,[32,32,32,[[32,[32,"  ",32,32],32,"  ",32,32],32,[32,"  ",32,32],32,"  ",32,32],[32,[32,"  ",32,32],32,"  ",32,32],32,[32,"  ",32,32],32,"  ",32,32],[60,60,"\"write_once_put_objsize_mean\"",62,62],44,10,[32,32,32,[[32,[32,"  ",32,32],32,"  ",32,32],32,[32,"  ",32,32],32,"  ",32,32],[32,[32,"  ",32,32],32,"  ",32,32],32,[32,"  ",32,32],32,"  ",32,32],[60,60,"\"write_once_put_objsize_median\"",62,62],44,10,[32,32,32,[[32,[32,"  ",32,32],32,"  ",32,32],32,[32,"  ",32,32],32,"  ",32,32],[32,[32,"  ",32,32],32,"  ",32,32],32,[32,"  ",32,32],32,"  ",32,32],[60,60,"\"write_once_put_time_100\"",62,62],44,10,[32,32,32,[[32,[32,"  ",32,32],32,"  ",32,32],32,[32,"  ",32,32],32,"  ",32,32],[32,[32,"  ",32,32],32,"  ",32,32],32,[32,"  ",32,32],32,"  ",32,32],[60,60,"\"write_once_put_time_95\"",62,62],44,10,[32,32,32,[[32,[32,"  ",32,32],32,"  ",32,32],32,[32,"  ",32,32],32,"  ",32,32],[32,[32,"  ",32,32],32,"  ",32,32],32,[32,"  ",32,32],32,"  ",32,32],[60,60,"\"write_once_put_time_99\"",62,62],44,10,[32,32,32,[[32,[32,"  ",32,32],32,"  ",32,32],32,[32,"  ",32,32],32,"  ",32,32],[32,[32,"  ",32,32],32,"  ",32,32],32,[32,"  ",32,32],32,"  ",32,32],[60,60,"\"write_once_put_time_mean\"",62,62],44,10,[32,32,32,[[32,[32,"  ",32,32],32,"  ",32,32],32,[32,"  ",32,32],32,"  ",32,32],[32,[32,"  ",32,32],32,"  ",32,32],32,[32,"  ",32,32],32,"  ",32,32],[60,60,"\"write_once_put_time_median\"",62,62],44,10,[32,32,32,[[32,[32,"  ",32,32],32,"  ",32,32],32,[32,"  ",32,32],32,"  ",32,32],[32,[32,"  ",32,32],32,"  ",32,32],32,[32,"  ",32,32],32,"  ",32,32],[60,60,"\"write_once_puts\"",62,62],44,10,[32,32,32,[[32,[32,"  ",32,32],32,"  ",32,32],32,[32,"  ",32,32],32,"  ",32,32],[32,[32,"  ",32,32],32,"  ",32,32],32,[32,"  ",32,32],32,"  ",32,32],[60,60,"\"write_once_puts_total\"",62,62],44,10,[32,32,32,[[32,[32,"  ",32,32],32,"  ",32,32],32,[32,"  ",32,32],32,"  ",32,32],[32,[32,"  ",32,32],32,"  ",32,32],32,[32,"  ",32,32],32,"  ",32,32],[60,60,"\"xmerl_version\"",62,62]],93],44,10,[32,32,[[32,[32,"  ",32,32],32,"  ",32,32],32,[32,"  ",32,32],32,"  ",32,32],[32,[32,"  ",32,32],32,"  ",32,32],32,[32,"  ",32,32],32,"  ",32,32],[91,[[60,60,"\"search_index_throughtput_one\"",62,62]],93]],125],32,105,115,32,110,111,116,32,101,113,117,97,108,32,116,111,32,101,120,112,101,99,116,101,100,32,118,97,108,117,101,32,[123,["[]",44,10,[32,32,[32,[[[32,"   ",32,32,32],32,"   ",32,32,32],[32,"   ",32,32,32],32,"   ",32,32,32],[[32,"   ",32,32,32],32,"   ",32,32,32],[32,"   ",32,32,32],32,"   ",32,32,32],32,[[[32,"   ",32,32,32],32,"   ",32,32,32],[32,"   ",32,32,32],32,"   ",32,32,32],[[32,"   ",32,32,32],32,"   ",32,32,32],[32,"   ",32,32,32],32,"   ",32,32,32],"[]"],125],32,97,116,32,108,105,110,101,32,"211"],<<"0h 0m 23.992132s">>],
           ["verify_object_limits","pass","N/A",<<"0h 0m 14.602560s">>],
           ["verify_leave","pass","N/A",<<"0h 4m 17.382353s">>],
           ["verify_handoff_mixed",fail,[85,110,107,110,111,119,110,32,101,114,114,111,114,32,101,110,99,111,117,110,116,101,114,101,100,58,32,"undef"],<<"0h 1m 2.126674s">>],
           ["verify_handoff_mixed",fail,[85,110,107,110,111,119,110,32,101,114,114,111,114,32,101,110,99,111,117,110,116,101,114,101,100,58,32,"undef"],<<"0h 1m 2.398744s">>],
           ["verify_down","pass","N/A",<<"0h 0m 53.3247s">>],
           ["verify_claimant","pass","N/A",<<"0h 1m 2.273822s">>],
           ["verify_capabilities","pass","N/A",<<"0h 2m 1.940995s">>],
           ["verify_build_cluster","pass","N/A",<<"0h 11m 47.760761s">>],
           ["verify_bitcask_tombstone2_upgrade","pass","N/A",<<"0h 0m 39.347898s">>],
           ["verify_aae","pass","N/A",<<"0h 1m 53.385164s">>],
           ["riak_control_authentication","pass","N/A",<<"0h 2m 8.613755s">>],
           ["replication_object_reformat","pass","N/A",<<"0h 19m 32.623379s">>],
           ["replication2_pg:test_pg_proxy_ssl","pass","N/A",<<"0h 5m 16.743773s">>],
           ["replication2_pg:test_pg_proxy","pass","N/A",<<"0h 3m 36.389589s">>],
           ["replication2_pg:test_multiple_sink_pg_ssl","pass","N/A",<<"0h 4m 55.247179s">>],
           ["replication2_pg:test_multiple_sink_pg","pass","N/A",<<"0h 3m 31.778789s">>],
           ["replication2_pg:test_mixed_pg_ssl","pass","N/A",<<"0h 4m 46.61629s">>],
           ["replication2_pg:test_mixed_pg","pass","N/A",<<"0h 3m 31.142957s">>],
           ["replication2_pg:test_bidirectional_pg_ssl","pass","N/A",<<"0h 8m 1.290814s">>],
           ["replication2_pg:test_bidirectional_pg","pass","N/A",<<"0h 6m 39.363728s">>],
           ["replication2_pg:test_basic_pg_mode_repl13","pass","N/A",<<"0h 6m 7.849937s">>],
           ["replication2_pg:test_basic_pg_mode_mixed_ssl","pass","N/A",<<"0h 7m 45.578932s">>],
           ["replication2_pg:test_basic_pg_mode_mixed","pass","N/A",<<"0h 6m 26.394490s">>]]),
        io:format(F, "Result = ~p~n", [Result]),
        file:close(F),
        ?assertEqual(Result, [[43,
            [["--",43],
                ["-",43],
                ["---------------------------------------------------------------------------------------------------------------",
                    43],
                ["-",43]],
            10],
            [124,"Te|","R|",
                "                                                    Reason                                                     |",
                "T|","\n"],
            [43,
                [["--",43],
                    ["-",43],
                    ["---------------------------------------------------------------------------------------------------------------",
                        43],
                    ["-",43]],
                10],
            [[124,"re|","p|",
                "                                                      N/A                                                      |",
                "0|","\n"]],
            [[124,"re|","p|",
                "                                                      N/A                                                      |",
                "0|","\n"]],
            [[124,"re|","p|",
                "                                                      N/A                                                      |",
                "0|","\n"]],
            [[124,"re|","p|",
                "                                                      N/A                                                      |",
                "0|","\n"]],
            [[124,"re|","p|",
                "                                                      N/A                                                      |",
                "0|","\n"]],
            [[124,"re|","p|",
                "                                                      N/A                                                      |",
                "0|","\n"]],
            [[124,"re|","p|",
                "                                                      N/A                                                      |",
                "0|","\n"]],
            [[124,"pb|","p|",
                "                                                      N/A                                                      |",
                "0|","\n"]],
            [[124,"pa|","f|",
                "                                     Unknown error encountered: {timeout,                                      |",
                "0|","\n"],
                [124,"  |"," |",
                    "                                                             {gen_server,call,                                 |",
                    " |","\n"],
                [124,"  |"," |",
                    "                                               [{riak_search_vnode_master,'dev3@127.0.0.1'},                   |",
                    " |","\n"],
                [124,"  |"," |",
                    "                                                             {riak_vnode_req_v1,                               |",
                    " |","\n"],
                [124,"  |"," |",
                    "                                              182687704666362864775460604089535377456991567872,                |",
                    " |","\n"],
                [124,"  |"," |",
                    "                                                        {server,undefined,undefined},                          |",
                    " |","\n"],
                [124,"  |"," |",
                    "                                                           {riak_core_fold_req_v1,                             |",
                    " |","\n"],
                [124,"  |"," |",
                    "                                                     #Fun<partition_repair.37.105725082>,                      |",
                    " |","\n"],
                [124,"  |"," |",
                    "                                                            {dict,0,16,16,8,80,48,                             |",
                    " |","\n"],
                [124,"  |"," |",
                    "                                                  {[],[],[],[],[],[],[],[],[],[],[],[],[],[],                  |",
                    " |","\n"],
                [124,"  |"," |",
                    "                                                                     [],[]},                                   |",
                    " |","\n"],
                [124,"  |"," |",
                    "                                                  {{[],[],[],[],[],[],[],[],[],[],[],[],[],[],                 |",
                    " |","\n"],
                [124,"  |"," |",
                    "                                                                   [],[]}}}}},                                 |",
                    " |","\n"],
                [124,"  |"," |",
                    "                                                                  600000]}}                                    |",
                    " |","\n"]],
            [[124,"lo|","p|",
                "                                                      N/A                                                      |",
                "0|","\n"]],
            [[124,"lo|","p|",
                "                                                      N/A                                                      |",
                "0|","\n"]],
            [[124,"jm|","f|",
                "                                   Unknown error encountered: {case_clause,                                    |",
                "0|","\n"],
                [124,"  |"," |",
                    "                               <<\"Exception in thread \\\"main\\\" java.io.IOException: Failed to retrieve RMIServe|",
                    " |","\n"]],
            [[124,"gh|","p|",
                "                                                      N/A                                                      |",
                "0|","\n"]],
            [[124,"en|","p|",
                "                                                      N/A                                                      |",
                "0|","\n"]],
            [[124,"en|","p|",
                "                                                      N/A                                                      |",
                "0|","\n"]],
            [[124,"ba|","p|",
                "                                                      N/A                                                      |",
                "0|","\n"]],
            [[124,"ve|","p|",
                "                                                      N/A                                                      |",
                "0|","\n"]],
            [[124,"ve|","p|",
                "                                                      N/A                                                      |",
                "0|","\n"]],
            [[124,"ve|","f|",
                "                { MissingStatsKeys , AdditionalStatsKeys } = {[<<\"search_index_latency_mean\">>,                |",
                "0|","\n"],
                [124,"  |"," |",
                    "                                                              <<\"search_index_throughput_one\">>,               |",
                    " |","\n"],
                [124,"  |"," |",
                    "                                                               <<\"search_query_latency_mean\">>,                |",
                    " |","\n"],
                [124,"  |"," |",
                    "                                                                   <<\"write_once_merge\">>,                     |",
                    " |","\n"],
                [124,"  |"," |",
                    "                                                              <<\"write_once_put_objsize_100\">>,                |",
                    " |","\n"],
                [124,"  |"," |",
                    "                                                               <<\"write_once_put_objsize_95\">>,                |",
                    " |","\n"],
                [124,"  |"," |",
                    "                                                               <<\"write_once_put_objsize_99\">>,                |",
                    " |","\n"],
                [124,"  |"," |",
                    "                                                              <<\"write_once_put_objsize_mean\">>,               |",
                    " |","\n"],
                [124,"  |"," |",
                    "                                                             <<\"write_once_put_objsize_median\">>,              |",
                    " |","\n"],
                [124,"  |"," |",
                    "                                                                <<\"write_once_put_time_100\">>,                 |",
                    " |","\n"],
                [124,"  |"," |",
                    "                                                                <<\"write_once_put_time_95\">>,                  |",
                    " |","\n"],
                [124,"  |"," |",
                    "                                                                <<\"write_once_put_time_99\">>,                  |",
                    " |","\n"],
                [124,"  |"," |",
                    "                                                               <<\"write_once_put_time_mean\">>,                 |",
                    " |","\n"],
                [124,"  |"," |",
                    "                                                              <<\"write_once_put_time_median\">>,                |",
                    " |","\n"],
                [124,"  |"," |",
                    "                                                                    <<\"write_once_puts\">>,                     |",
                    " |","\n"],
                [124,"  |"," |",
                    "                                                                 <<\"write_once_puts_total\">>,                  |",
                    " |","\n"],
                [124,"  |"," |",
                    "                                                                    <<\"xmerl_version\">>],                      |",
                    " |","\n"],
                [124,"  |"," |",
                    "                                              [<<\"search_index_throughtput_one\">>]} is not equal to expected va|",
                    " |","\n"],
                [124,"  |"," |",
                    "                                                                                                               |",
                    " |","\n"]],
            [[124,"ve|","p|",
                "                                                      N/A                                                      |",
                "0|","\n"]],
            [[124,"ve|","p|",
                "                                                      N/A                                                      |",
                "0|","\n"]],
            [[124,"ve|","f|",
                "                                       Unknown error encountered: undef                                        |",
                "0|","\n"]],
            [[124,"ve|","f|",
                "                                       Unknown error encountered: undef                                        |",
                "0|","\n"]],
            [[124,"ve|","p|",
                "                                                      N/A                                                      |",
                "0|","\n"]],
            [[124,"ve|","p|",
                "                                                      N/A                                                      |",
                "0|","\n"]],
            [[124,"ve|","p|",
                "                                                      N/A                                                      |",
                "0|","\n"]],
            [[124,"ve|","p|",
                "                                                      N/A                                                      |",
                "0|","\n"]],
            [[124,"ve|","p|",
                "                                                      N/A                                                      |",
                "0|","\n"]],
            [[124,"ve|","p|",
                "                                                      N/A                                                      |",
                "0|","\n"]],
            [[124,"ri|","p|",
                "                                                      N/A                                                      |",
                "0|","\n"]],
            [[124,"re|","p|",
                "                                                      N/A                                                      |",
                "0|","\n"]],
            [[124,"re|","p|",
                "                                                      N/A                                                      |",
                "0|","\n"]],
            [[124,"re|","p|",
                "                                                      N/A                                                      |",
                "0|","\n"]],
            [[124,"re|","p|",
                "                                                      N/A                                                      |",
                "0|","\n"]],
            [[124,"re|","p|",
                "                                                      N/A                                                      |",
                "0|","\n"]],
            [[124,"re|","p|",
                "                                                      N/A                                                      |",
                "0|","\n"]],
            [[124,"re|","p|",
                "                                                      N/A                                                      |",
                "0|","\n"]],
            [[124,"re|","p|",
                "                                                      N/A                                                      |",
                "0|","\n"]],
            [[124,"re|","p|",
                "                                                      N/A                                                      |",
                "0|","\n"]],
            [[124,"re|","p|",
                "                                                      N/A                                                      |",
                "0|","\n"]],
            [[124,"re|","p|",
                "                                                      N/A                                                      |",
                "0|","\n"]],
            [[124,"re|","p|",
                "                                                      N/A                                                      |",
                "0|","\n"]],
            [43,
                [["--",43],
                    ["-",43],
                    ["---------------------------------------------------------------------------------------------------------------",
                        43],
                    ["-",43]],
                10]])
        end).
-endif.
