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
-module(clique_typecast).

-export([to_node/1]).

%% This typecast automatically checks if the node is in our cluster,
%% since the atom will have to be registered
-spec to_node(string()) -> node() | {error, bad_node}.
to_node(Str) ->
    try
        Node = list_to_existing_atom(Str),
        case lists:member(Node, clique_nodes:nodes()) of
            true ->
                Node;
            false ->
                {error, bad_node}
        end
    catch error:badarg ->
        {error, bad_node}
    end.
