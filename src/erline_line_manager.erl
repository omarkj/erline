-module(erline_line_manager).
-behaviour(gen_server).
-include("erline.hrl").

-export([init/1,
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 terminate/2, 
	 code_change/3]).

-export([start_link/4,
	 action_return/2]).

-record(state, {ref :: reference(),
		caller :: pid()|atom(),
		actions_left = 0 :: integer(),
		results = [] :: []|[any()],
		pipelines = [] :: [] | [#pipeline{}],
		done_pipelines = [] :: [] | [#pipeline{}],
		input :: any()}).

start_link(Caller, Ref, Pipelines, Input) ->
    gen_server:start_link(?MODULE, [Caller, Ref, Pipelines, Input], []).

action_return(Pid, Result) ->
    gen_server:call(Pid, {action_over, Result}).

init([Caller, Ref, Pipelines, Input]) ->
    try initialize_pipelines(Pipelines, []) of
	Pipelines0 ->
	    {ok, reset_state(Pipelines0, Input, #state{caller=Caller,
						       ref=Ref})}
    catch
	_:_ = Err ->
	    {stop, Err}
    end.

handle_call({action_over, Result}, _From, #state{caller=Caller,
						 ref=Ref,
						 pipelines=[],
						 results=Results,
						 actions_left=1,
						 done_pipelines=Done}) ->
    return_reply(Caller, Ref, Result, Results),
    {stop, normal, ok, Done};
handle_call({action_over, Result}, _From, #state{results=Results,
						 actions_left=1,
						 pipelines=Pipelines}=State) ->
    {reply, ok, reset_state(Pipelines, Result, Results, State)};
handle_call({action_over, Result}, _From, #state{results=Results,
						 actions_left=ActionsLeft}=State) ->
    {reply, ok, State#state{actions_left=ActionsLeft-1,
			    results=Results++[Result]}};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, []) ->
    Reason;
terminate(_Reason, [#pipeline{end_fun=F, init_return=Init}|Rest]) when is_function(F) ->
    ?RUN_OR_LOG(F(Init), "Error running end of pipeline function ~p"),
    terminate(_Reason, Rest);
terminate(_Reason, [#pipeline{end_fun={M,F}, init_return=Init}|Rest]) ->
    ?RUN_OR_LOG(M:F(Init), "Error running end of pipeline function ~p"),
    terminate(_Reason, Rest);
terminate(_Reason, [_|Rest]) ->
    terminate(_Reason, Rest).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

reset_state(Pipelines, Result, [], State) ->
    reset_state(Pipelines, Result, State);
reset_state(Pipelines, Result, Results, State) ->
    reset_state(Pipelines, Results++[Result], State).

reset_state([#pipeline{type=Type, actions=Actions, init_return=I}=Pipeline|Rest], Input,
	    #state{done_pipelines=DonePipelines}=State) ->
    RunningActions = start_actions(Type, Actions, [{init_with, I}], Input),
    State#state{pipelines=Rest,
		done_pipelines=DonePipelines++[Pipeline],
		actions_left=length(RunningActions)}.
    
start_actions(sequential, Actions, ActionOpts, Input) ->
    [erline_action_sup:start_action(Actions, ActionOpts, Input, true)];
start_actions(concurrent, Actions, Opts, Input) ->
    lists:map(fun(Action) ->
		      [Pid] = start_actions(sequential, Action, Opts, Input),
		      Pid
	      end, Actions).

%% @TODO check if the init_return is set, and if that's the fact do not
%% run the on_start - then we are workin in a "warm" pipeline.
initialize_pipelines([], Res) ->
    Res;
initialize_pipelines([#pipeline{opts=Opts}=Pipeline|Pipelines], Res) ->
    Pipeline0 = handle_opts(Opts, Pipeline),
    initialize_pipelines(Pipelines, Res++[Pipeline0]).

handle_opts([], Pipeline) ->
    Pipeline;
handle_opts([{on_start, {M,F,A}}|Opts], Pipeline) ->
    handle_opts(Opts, Pipeline#pipeline{init_return=M:F(A)});
handle_opts([{on_start, F}|Opts], Pipeline) ->
    handle_opts(Opts, Pipeline#pipeline{init_return=F()});
handle_opts([{on_end, {M, F}}|Opts], Pipeline) ->
    handle_opts(Opts, Pipeline#pipeline{end_fun={M,F}});
handle_opts([{on_end, F}|Opts], Pipeline) ->
    handle_opts(Opts, Pipeline#pipeline{end_fun=F}).

return_reply(Caller, Ref, Result, []) ->
    Caller ! {Ref, Result};
return_reply(Caller, Ref, Result, Results) ->
    Caller ! {Ref, Results++[Result]}.
