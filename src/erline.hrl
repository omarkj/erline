-type pipeline_opt() :: {on_start, function()|mfa()}|
			{on_end, function()|{module(),function()}}.
-type pipeline_opts() :: [pipeline_opts()]|[].

-type pipeline_type() :: parallel|sequential.
-record(pipeline, {type :: pipeline_type(),
		   opts :: term()|undefined,
		   init_return :: any()|undefined,
		   end_fun :: function()|{module(),function()},
		   actions = [] :: [module()|[#pipeline{}]|function()]|[]}).
-type pipeline() :: list(#pipeline{}).

-define(RUN_OR_LOG(Fun, Format), try Fun of _ ->
					 ok
				 catch _:_=Error ->
					 error_logger:error_msg(Format, [Error])
				 end).
