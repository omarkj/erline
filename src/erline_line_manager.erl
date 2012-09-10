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
		next_pipeline :: #pipeline{},
		finally :: #pipeline{},
		input :: any()}).

start_link(Caller, Ref, Pipeline, Input) ->
    gen_server:start_link(?MODULE, [Caller, Ref, Pipeline, Input], []).

action_return(Pid, Result) ->
    gen_server:call(Pid, {action_over, Result}).

init([Caller, Ref, #pipeline{nextline=Nextline,
			     type=Type,
			     actions=Actions,
			     opts=Opts,
			     finally=Finally}, Input]) ->
    erline_action_sup:start_action(Type, Actions, Opts, Input),
    {ok, #state{ref=Ref,
		caller=Caller,
		next_pipeline=Nextline,
		finally=Finally}}.

handle_call({action_over, Results}, _From, #state{next_pipeline=undefined,
						  caller=Caller,
						  ref=Ref,
						  finally=undefined}=State) ->
    Caller ! {Ref, Results},
    {stop, normal, ok, State};
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
