%% @doc Implements an io server that can be used as a group leader in
%% testing, and supports changing the size of the io device.
-module(clique_test_group_leader).

-export([new_group_leader/0,
         get_output/1,
         set_size/3,
         stop/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-behaviour(gen_server).

-record(state, {output=[], size={80,20}}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%%-----------------
%% API Functions
%%----------------

%% @doc Spawns the new group leader and makes it the current
%% group_leader, linking it to the current process.
-spec new_group_leader() -> pid().
new_group_leader() ->
    %% OldLeader = group_leader(),
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    group_leader(Pid, self()),
    Pid.

%% @doc Gets the output captured by the group leader.
-spec get_output(pid()) -> iodata().
get_output(Pid) ->
    gen_server:call(Pid, get_output, infinity).

%% @doc Sets the dimensions of the group leader's output.
%% @see io:columns/0
%% @see io:rows/0
-spec set_size(pid(), pos_integer(), pos_integer()) -> ok.
set_size(Pid, Cols, Rows) ->
    gen_server:call(Pid, {set_size, Cols, Rows}, infinity).

%% @doc Stops the group leader process.
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:cast(Pid, stop).

%%-----------------
%% gen_server callbacks
%%----------------

init([]) ->
    {ok, #state{}}.

handle_call(get_output, _From, #state{output=Out}=State) ->
    {reply, lists:reverse(Out), State};
handle_call({set_size, Cols, Rows}, _From, State) when is_integer(Cols),
                                                       is_integer(Rows),
                                                       Cols > 0,
                                                       Rows > 0 ->
    {reply, ok, State#state{size={Cols, Rows}}};
handle_call({set_size, _, _}, _From, State) ->
    {reply, {error, invalid_size}, State};
handle_call(_, _, State) ->
    {reply, {error, bad_call}, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info({io_request, From, ReplyAs, Req}, State) ->
    P = process_flag(priority, normal),
    %% run this part under normal priority always
    NewState = io_request(From, ReplyAs, Req, State),
    process_flag(priority, P),
    {noreply, NewState};
handle_info(_, State) ->
    {noreply, State}.

terminate(_, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------
%% Internal API
%%----------------
io_request(From, ReplyAs, Req, State) ->
    {Reply, NewState} = io_request(Req, State),
    _ = io_reply(From, ReplyAs, Reply),
    NewState.

%% sends a reply back to the sending process
io_reply(From, ReplyAs, Reply) ->
    From ! {io_reply, ReplyAs, Reply}.

%% Handles io requests
%% Output:
io_request({put_chars, Chars},
           #state{output=O}=State) when is_binary(Chars);
                                        is_list(Chars) ->
    {ok, State#state{output=[Chars|O]}};
io_request({put_chars, M, F, A}, State) ->
    try apply(M, F, A) of
        Chars ->
            io_request({put_chars, Chars}, State)
    catch C:T ->
            {{error, {C,T, erlang:get_stacktrace()}}, State}
    end;
io_request({put_chars, _Enc, Chars}, State) ->
    io_request({put_chars, Chars}, State);
io_request({put_chars, _Enc, Mod, Func, Args}, State) ->
    io_request({put_chars, Mod, Func, Args}, State);
%% Terminal geometry:
io_request({get_geometry,columns}, #state{size={Cols,_}}=State) ->
    {Cols, State};
io_request({get_geometry,rows}, #state{size={_,Rows}}=State) ->
    {Rows, State};
%% Input (unsupported):
io_request({get_chars, _Enc, _Prompt, _N}, State) ->
    {eof, State};
io_request({get_chars, _Prompt, _N}, State) ->
    {eof, State};
io_request({get_line, _Prompt}, State) ->
    {eof, State};
io_request({get_line, _Enc, _Prompt}, State) ->
    {eof, State};
io_request({get_until, _Prompt, _M, _F, _As}, State) ->
    {eof, State};
%% Options:
io_request({setopts, _Opts}, State) ->
    {{error, enotsup}, State};
io_request(getopts, State) ->
    {{error, enotsup}, State};
%% Multi-requests:
io_request({requests, Reqs}, State) ->
    io_requests(Reqs, {ok, State});
%% Ignore all other messages:
io_request(_, State) ->
    {{error, request}, State}.

io_requests([R | Rs], {ok, State}) ->
    io_requests(Rs, io_request(R, State));
io_requests(_, Result) ->
    Result.

-ifdef(TEST).
-define(GL(T),
        begin
            Res = setup(),
            try
                T
            after
                cleanup(Res)
            end
        end).

setup() ->
    OldLeader = group_leader(),
    Leader = new_group_leader(),
    {OldLeader, Leader}.

cleanup({OldLeader, Leader}) ->
    group_leader(OldLeader, self()),
    io:format("CAPTURED: ~s", [get_output(Leader)]),
    stop(Leader).

leader_test_() ->
     [
      {"group leader captures output, readable with get_output", fun test_capture/0},
      {"group leader does not support input", fun test_no_input/0},
      {"set_size/3 changes IO geometry", fun test_set_size/0}
     ].

test_capture() ->
    ?GL(begin
            ?assertEqual(ok, io:put_chars(<<"line 1\n">>)),
            ?assertEqual(ok, io:put_chars(<<"line 2\n">>)),
            ?assertEqual([<<"line 1\n">>,<<"line 2\n">>],
                         get_output(group_leader()))
        end).

test_set_size() ->
    ?GL(begin
            ?assertEqual({ok, 80}, io:columns()),
            ?assertEqual({ok, 20}, io:rows()),
            set_size(group_leader(), 202, 29),
            ?assertEqual({ok, 202}, io:columns()),
            ?assertEqual({ok, 29}, io:rows())
        end).

test_no_input() ->
    ?GL(begin
            ?assertEqual(ok, io:put_chars(<<"line 1\n">>)),
            ?assertEqual(eof, io:get_chars("> ", 10))
        end).
-endif.
