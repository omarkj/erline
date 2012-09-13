-module(erline).
-export([create/3,
	 sync/2,
	 async/2]).
-include("erline.hrl").

create(sequential, Actions, Options) ->
    create(#pipeline{type=sequential}, Actions, Options);
create(concurrent, Actions, Options) ->
    create(#pipeline{type=concurrent}, Actions, Options);
create(#pipeline{}=Pipeline, Actions, Options) ->
    [Pipeline#pipeline{actions=validate_actions(Actions, []),
		       opts=Options}].

validate_actions([], Res) ->
    Res;
validate_actions([Function|Rest], Res) when is_function(Function) ->
    validate_actions(Rest, Res++[Function]);
validate_actions([Module|Rest], Res) when is_atom(Module) ->
    case erlang:module_loaded(Module) of
	true -> validate_actions(Rest, Res++[Module]);
	_ -> erlang:error(badarg)
    end;
validate_actions([[#pipeline{}|_]=Pipelines|Rest], Res) ->
    validate_actions(Rest, Res++[Pipelines]).

sync(Pipelines, Input) ->
    Ref = async(Pipelines, Input),
    receive
	{Ref, Res} ->
	    Res
    end.

async([#pipeline{}|_]=Pipelines, Input) ->
    erline_manager_sup:start_pipeline(self(), Pipelines, Input).

