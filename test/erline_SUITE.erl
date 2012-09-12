-module(erline_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).

-export([init_per_suite/1,
	 end_per_suite/1]).

-export([simple_line/1,
	 seq_line_with_line/1,
	 module_line/1]).

all() ->    
    [simple_line
     ,seq_line_with_line
     ,module_line
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
    %erline:sync(erline:prepare(S), <<"test">>),
    <<"testaddon1addon2">> = erline:sync(erline:prepare(S), <<"test">>),
%    rambo = erline:sync(erline:prepare(S), <<"test">>),
    Config.
