-module(erline_SUITE_module1).

-export([handle/1]).

handle(Binary) ->
    <<Binary/binary, "addon1">>.
