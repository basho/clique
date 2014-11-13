%% @doc The following types describe an abstract format for status information.
%% Each type has a semantic, organizational meaning in a way similar to an html
%% document. The difference here is that we want our format to use erlang
%% data structures and types and be able to generate human readable output, json,
%% csv and a subset of html, as well as other possible output.
-type text() :: {text, iolist()}.
-type column() :: {column, iolist(), [iolist()]}.
-type table() :: {table, [{iolist(), iolist()}]}.
-type alert() :: {alert, [column() | table() | text()]}.
-type elem() :: text() | column() | table() | alert().
-type status() :: [elem()].
