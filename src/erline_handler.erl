-module(erline_handler).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{init,2},
     {handle,2},
     {crash,2}];
behaviour_info(_) ->
    undefined.

