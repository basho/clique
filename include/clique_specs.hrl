%% This record represents the specification for a key-value argument
%% or flag on the command line.
-record(clique_spec,
        {
          key        :: atom(),
          name       :: string(),
          shortname  :: char() | undefined,
          datatype   :: cuttlefish_datatypes:datatype() | undefined,
          validator  :: fun((term()) -> ok | err()) | undefined,
          typecast   :: fun((string()) -> err() | term()) | undefined
        }).

-type spec() :: #clique_spec{}.
