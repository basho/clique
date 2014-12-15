%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.  All Rights Reserved.
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
-module(clique_sup).
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
    {ok, {{one_for_one, 0, 10}, [?CHILD(clique_manager, worker)]}}.
