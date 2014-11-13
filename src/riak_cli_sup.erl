-module(riak_cli_sup).
-behaviour(supervisor).

%% beahvior functions
-export([start_link/0,
         init/1
        ]).

-define(CHILD(I,Type), {I,{I,start_link,[]},permanent,brutal_kill,Type,[I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    %% We want to take down the node if the process gets killed. The process
    %% does nothing besides create ets tables and register cuttlefish schemas.
    %% If we lose the tables we lose cli access. Therefore
    %% riak_core_console_manager should do no work outside of init/1.
    {ok, {{one_for_one, 0, 10}, [?CHILD(riak_cli_manager, worker)]}}.
