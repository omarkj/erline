-module(erline_SUITE_module2).

-export([handle/1]).

handle(Binary) ->
    <<Binary/binary, "addon2">>.
