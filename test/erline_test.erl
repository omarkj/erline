-module(erline_test).
-include("../src/erline.hrl").
-include_lib("eunit/include/eunit.hrl").

erline_test_() ->
    {setup,
     fun() ->
	     application:start(meck),
	     application:start(erline)
     end,
     fun(_Pid) ->
	     application:stop(meck),
	     application:stop(erline)
     end,
     [
      {"check a spec", ?_test(t_check_spec())}
     ]}.

% Tests
t_check_spec() ->
    lists:foreach(fun(ModuleName) ->
			  meck:new(ModuleName)
		  end, [module1,module2,module3,module4,
			module5,module6]),
    Pipeline0 = erline:create(concurrent, [module3, fun(D) -> D end,
					   module4], []),
    Pipeline1 = erline:create(sequential, [module5], []),
    Pipeline2 = erline:create(sequential, [module1,
					   erline:create(sequential, [module2], [])], []),
    ?assertMatch([#pipeline{type=concurrent,
			    actions=[module3,_,module4],
			    opts=[]}], Pipeline0),
    ?assertEqual([#pipeline{type=sequential,
			    actions=[module5],
			    opts=[]}], Pipeline1),
    ?assertEqual([#pipeline{type=sequential,
			    actions=[module1,
				     [#pipeline{type=sequential,
						actions=[module2],
						opts=[]}]],
			    opts=[]}], Pipeline2).
