-type pipeline_type() :: parallel|sequential.
-record(pipeline, {type :: pipeline_type(),
		   opts :: term()|undefined,
		   actions = [] :: [module()|[#pipeline{}]|function()]|[]}).
-type pipeline() :: list(#pipeline{}).
