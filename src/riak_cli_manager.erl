-module(riak_cli_manager).

-behaviour(gen_server).

%% gen_server api and callbacks
-export([start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Note that this gen_server only exists to create ets tables and keep them
%% around indefinitely. If it dies once, the node will die, as riak-admin will
%% functionality will no longer be available. However, since it discards all
%% messages, it can only die if explicitly killed.
%%
init([]) ->
    ok = riak_cli_command:init(),
    ok = riak_cli_usage:init(),
    ok = riak_cli_config:init(),
    ok = riak_cli_nodes:init(),
    {ok, #state{}}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.
handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Msg, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

