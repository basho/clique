%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015 Basho Technologies, Inc.
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
%% @doc Implements an io server that can be used as a group leader in
%% testing, and supports changing the size of the io device.
%% -------------------------------------------------------------------

-define(TEST_GROUP_LEADER(T),
    begin
        Res = setup(),
        try
            T
        after
            cleanup(Res)
        end
    end).


setup() ->
    OldLeader = erlang:group_leader(),
    Leader = clique_test_group_leader:new_group_leader(),
    {OldLeader, Leader}.

cleanup({OldLeader, Leader}) ->
    erlang:group_leader(OldLeader, self()),
    io:format("CAPTURED: ~s", [clique_test_group_leader:get_output(Leader)]),
    clique_test_group_leader:stop(Leader).
