%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015 Basho Technologies, Inc.  All Rights Reserved.
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

%% @doc Functions related to specifications of key-value arguments and
%% flags.
-module(clique_spec).
-include("clique_specs.hrl").

-export([
         make/1,
         key/1,
         name/1,
         shortname/1,
         datatype/1,
         validator/1,
         typecast/1
        ]).

-type err() :: {error, term()}.

%% @doc Creates a spec from a list of options.
make({Key, Options}) ->
    Shortname = case proplists:get_value(shortname, Options) of
                    %% Unwrap the shortname character so we can match
                    %% more efficiently in the parser.
                    [Char] -> Char;
                    _ -> undefined
                end,
    #clique_spec{
       key = Key,
       name = proplists:get_value(longname, Options, atom_to_list(Key)),
       shortname = Shortname,
       datatype = proplists:get_value(datatype, Options, string),
       validator = proplists:get_value(validator, Options),
       typecast = proplists:get_value(typecast, Options)
      }.

key(#clique_spec{key=V}) -> V.

name(#clique_spec{name=V}) -> V.

shortname(#clique_spec{shortname=V}) -> V.

datatype(#clique_spec{datatype=V}) -> V.

validator(#clique_spec{validator=V}) -> V.

typecast(#clique_spec{typecast=V}) -> V.
