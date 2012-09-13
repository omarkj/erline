-module(erline).
-export([create/3,
	 sync/2,
	 async/2]).
-include("erline.hrl").

-export_type([pipeline/0]).

%% @doc
%% Create a new pipeline. This doesn't start it, only
%% validates it and returns a pipeline that can be run using
%% @see sync/2. and @see async/2.
%% @end
-spec create(sequential|concurrent,
	     [module()|pipeline()|function()]|[],
	     []) -> pipeline().
create(sequential, Actions, Options) ->
    create(#pipeline{type=sequential}, Actions, Options);
create(concurrent, Actions, Options) ->
    create(#pipeline{type=concurrent}, Actions, Options);
create(#pipeline{}=Pipeline, Actions, Options) ->
    [Pipeline#pipeline{actions=validate_actions(Actions, []),
		       opts=Options}].

%% @doc
%% Runs the pipeline and wait for it to finish.
%% @end
-spec sync(pipeline(), any()) -> [any()].
sync(Pipelines, Input) ->
    Ref = async(Pipelines, Input),
    receive
	{Ref, Res} ->
	    Res
    end.

%% @doc
%% Runs the pipeline asyncly, returns a reference that
%% will be returned to the calling process in a tuple
%% looking like this
%% ```{Ref, Results}'''
%% @TODO monitor to the running manager to be able to get
%% a notification if it crashes during the run.
%% @end
-spec async(pipeline(), any()) -> reference().
async([#pipeline{}|_]=Pipelines, Input) ->
    erline_manager_sup:start_pipeline(self(), Pipelines, Input).

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
