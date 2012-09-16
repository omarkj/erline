-module(erline_SUITE_module4).

-export([handle/1]).

handle(Atom) ->
    {module4, atom_to_list(Atom)}.
