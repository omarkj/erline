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
	     pipeline_opts()) -> pipeline().
create(sequential, Actions, Options) ->
    create(#pipeline{type=sequential}, Actions, Options);
create(concurrent, Actions, Options) ->
    create(#pipeline{type=concurrent}, Actions, Options);
create(#pipeline{}=Pipeline, Actions, Options) ->
    [Pipeline#pipeline{actions=validate_actions(Actions, []),
		       opts=validate_opts(Options, [])}].

%% @doc
%% Runs the pipeline and wait for it to finish.
%% @TODO add timeout?
%% @end
-spec sync(pipeline(), any()) -> [any()].
sync(Pipelines, Input) ->
    case async(Pipelines, Input) of
	{error, Error} ->
	    {error, Error};
	{_Pid, Ref} ->
	    receive
		{Ref, Res} ->
		    Res
	    end
    end.

%% @doc
%% Runs the pipeline asyncly, returns a reference and pid that
%% will be returned to the calling process in a tuple
%% looking like this
%% ```{Ref, Results}'''
%% @end
-spec async(pipeline(), any()) -> reference().
async([#pipeline{}|_]=Pipelines, Input) ->
    erline_manager_sup:start_pipeline(self(), Pipelines, Input).

%% Internal
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

validate_opts([], Res) ->
    Res;
validate_opts([{on_start, {M,F,_}}=Opt|Options], Res) when is_atom(M), is_atom(F) ->
    case erlang:function_exported(M, F, 1) of
	true -> validate_opts(Options, Res++[Opt]);
       _ -> erlang:error(badarg)
    end;
validate_opts([{on_start, F}=Opt|Options], Res) when is_function(F) ->
    validate_opts(Options, Res++[Opt]);
validate_opts([{on_end, {M,F}}=Opt|Options], Res) when is_atom(M), is_atom(F) ->
    case erlang:function_exported(M, F, 1) of
	true -> validate_opts(Options, Res++[Opt]);
       _ -> erlang:error(badarg)
    end;
validate_opts([{on_end, F}=Opt|Options], Res) when is_function(F) ->
    validate_opts(Options, Res++[Opt]).
