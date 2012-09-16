-type pipeline_opt() :: {on_start, function()|mfa()}|
			{on_end, function()|mfa()}.
-type pipeline_opts() :: [pipeline_opts()]|[].

-type pipeline_type() :: parallel|sequential.
-record(pipeline, {type :: pipeline_type(),
		   opts :: term()|undefined,
		   init_return :: any()|undefined,
		   actions = [] :: [module()|[#pipeline{}]|function()]|[]}).
-type pipeline() :: list(#pipeline{}).
