-module(erline_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).

-export([init_per_suite/1,
	 end_per_suite/1]).

-export([simple_line/1
	 ,seq_line_with_line/1
	 ,module_line/1
	 ,pipeline_to_pipeline/1
	 ,concurrent_line/1
	]).

all() ->    
    [
     simple_line
     ,seq_line_with_line
     ,module_line
     ,pipeline_to_pipeline
     ,concurrent_line
    ].

% Setup & teardown
init_per_suite(Config) ->
    ok = application:start(erline),
    Config.

end_per_suite(Config) ->
    ok = application:stop(erline),
    Config.

simple_line(Config) ->
    S = {sequential,[fun(X) -> X * 10 end,
		     fun(X) -> X * 12 end]},
    1800 = erline:sync(erline:prepare(S), 15),
    Config.

seq_line_with_line(Config) ->
    S = {sequential,[fun(X) -> X * 10 end,
		     {sequential,[fun(X) -> X * 10 end,
				  {sequential,[fun(X) -> X * 10 end,
					       fun(X) -> X * 15 end]},
				  fun(X) -> X * 15 end]},
		     fun(X) -> X * 12 end]},
    40500000 = erline:sync(erline:prepare(S), 15),
    Config.

module_line(Config) ->
    S = {sequential,[erline_SUITE_module1,
		     erline_SUITE_module2]},
    <<"testaddon1addon2">> = erline:sync(erline:prepare(S), <<"test">>),
    Config.

pipeline_to_pipeline(Config) ->
    P0 = {sequential,[fun(X) ->
			      X + 1
		      end,
		      fun(X) ->
			      X + 1
		      end]},
    P1 = erline:prepare(P0),
    5 = erline:sync(P1++P1, 1),
    Config.

concurrent_line(Config) ->
    Tab = ets:new(test, [public]),
    S = {concurrent, [fun({Tab1,X}) ->
			      F = X * 10,
			      ets:insert(Tab1, {fun_1, F})
		      end,
		      fun({Tab1,X}) ->
			      F = X * 12,
			      ets:insert(Tab1, {fun_2, F})
			      
		      end]},
    [true,true] = erline:sync(erline:prepare(S), {Tab,5}),
    [{fun_1, 50}] = ets:lookup(Tab, fun_1),
    [{fun_2, 60}] = ets:lookup(Tab, fun_2),
    Config.
