-module(erline_SUITE_module3).

-export([handle/1,
	 on_crash/1]).

handle(Binary) ->
    <<Binary/binary, "addon2">>.

on_crash(Reason) ->
    Reason.
