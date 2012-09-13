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
		pipelines = [] :: [] | [#pipeline{}] ,
		input :: any()}).

start_link(Caller, Ref, Pipelines, Input) ->
    gen_server:start_link(?MODULE, [Caller, Ref, Pipelines, Input], []).

action_return(Pid, Result) ->
    gen_server:call(Pid, {action_over, Result}).

init([Caller, Ref, Pipelines, Input]) ->
    {ok, reset_state(Pipelines, Input, #state{caller=Caller,
					      ref=Ref})}.

handle_call({action_over, Result}, _From, #state{caller=Caller,
						 ref=Ref,
						 pipelines=[],
						 results=[],
						 actions_left=1}) ->
    Caller ! {Ref, Result},
    {stop, normal, ok, undefined};
handle_call({action_over, Result}, _From, #state{caller=Caller,
						 ref=Ref,
						 pipelines=[],
						 results=Results,
						 actions_left=1}) ->
    Caller ! {Ref, Results++[Result]},
    {stop, normal, ok, undefined};
handle_call({action_over, Result}, _From, #state{results=[],
						 actions_left=1,
						 pipelines=Pipelines}=State) ->
    {reply, ok, reset_state(Pipelines, Result, State)};
handle_call({action_over, Result}, _From, #state{results=Results,
						 actions_left=1,
						 pipelines=Pipelines}=State) ->
    {reply, ok, reset_state(Pipelines, Results++[Result], State)};
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

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

reset_state([#pipeline{type=Type, actions=Actions, opts=Opts}|Rest], Input,
	    State) ->
    RunningActions = case start_line(Type, Actions, Opts, Input) of
			 Pid when is_pid(Pid) ->
			     1;
			 List ->
			     length(List)
		     end,
    State#state{pipelines=Rest,
		actions_left=RunningActions}.
    

start_line(sequential, Actions, Opts, Input) ->
    erline_action_sup:start_action(Actions, Opts, Input, true);
start_line(concurrent, Actions, Opts, Input) ->
    lists:map(fun(Action) -> start_line(sequential, Action, Opts, Input) end, Actions).
