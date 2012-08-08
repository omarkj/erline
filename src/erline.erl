-module(erline).
-export([prepare/2,
	 prepare/1]).
-include("erline.hrl").
-define(IF(Bool, A, B), if Bool -> A; true -> B end).

prepare(Pipeline) ->
    prepare(Pipeline, undefined).

prepare(Pipeline, Opts) ->
    Size = size(Pipeline),
    #pipeline{type=element(1, Pipeline),
	      actions=validate_actions(element(2, Pipeline), []),
	      nextline=?IF(Size >= 3, prepare(element(3, Pipeline), [inherit]),
			   undefined),
	      finally=?IF(Size >= 4, prepare(element(4, Pipeline), [inherit]),
			  undefined),
	      opts=Opts}.

validate_actions([], Res) ->
    Res;
validate_actions([Function|Rest], Res) when is_function(Function) ->
    validate_actions(Rest, Res++[Function]);
validate_actions([Module|Rest], Res) when is_atom(Module) ->
    case erlang:module_loaded(Module) of
	true -> validate_actions(Rest, Res++[Module]);
	_ -> erlang:error(badarg)
    end;
validate_actions([Pipeline|Rest], Res) when is_tuple(Pipeline) ->
    validate_actions(Rest, Res++[prepare(Pipeline, [inherit])]).
