%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014-2017 Basho Technologies, Inc.
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

-module(clique_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    clique_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% Tests
%% ===================================================================
-ifdef(TEST).

start_stop_test_() ->
    {setup,
        fun() ->
            LogDir = clique:create_test_dir(),
            ConLog = filename:join(LogDir, "console.log"),
            ErrLog = filename:join(LogDir, "error.log"),
            CrashLog = filename:join(LogDir, "crash.log"),
            application:load(sasl),
            application:set_env(sasl, errlog_type, error),
            application:load(lager),
            application:set_env(lager, crash_log, CrashLog),
            application:set_env(lager, handlers, [
                {lager_console_backend, warn},
                {lager_file_backend, [{file, ErrLog}, {level, warn}]},
                {lager_file_backend, [{file, ConLog}, {level, debug}]}]),
            _ = clique:ensure_stopped(),
            LogDir
        end,
        fun clique:delete_test_dir/1,
        fun() ->
            Ret = application:ensure_all_started(clique),
            ?assertMatch({ok, _}, Ret),
            {_, Started} = Ret,
            lists:foreach(fun(App) ->
                ?assertEqual(ok, application:stop(App))
            end, lists:reverse(Started))
        end}.

-endif. % TEST
