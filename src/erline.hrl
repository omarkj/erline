-type pipeline_type() :: parallel|sequential.

-type pipeline() :: {pipeline_type(),
		     [module()]|[pipeline()]|[],
		     pipeline(),
		     {finally, pipeline()}}|
		    {pipeline_type(),
		     [module()]|[pipeline()]|[],
		     pipeline()}.
		    
-record(pipeline, {type :: pipeline_type(),
		   opts :: term()|undefined,
		   actions = [] :: []|[module()]|[pipeline()]|
				   maybe_improper_list(module(),pipeline())}).
