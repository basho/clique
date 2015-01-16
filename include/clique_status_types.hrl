%% @doc The following types describe an abstract format for status information.
%% Each type has a semantic, organizational meaning in a way similar to an html
%% document. The difference here is that we want our format to use erlang
%% data structures and types and be able to generate human readable output, json,
%% csv and a subset of html, as well as other possible output.
-type text() :: {text, iolist()}.
-type status_list() :: {list, iolist(), [iolist()]} | {list, [iolist()]}.
-type table() :: {table, [[{atom() | string(), term()}]]}.
-type alert() :: {alert, [status_list() | table() | text()]}.
-type usage() :: usage.
-type elem() :: text() | status_list() | table() | alert() | usage().
-type status() :: [elem()].
