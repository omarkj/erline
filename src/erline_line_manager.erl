-module(erline_line_manager).
-behaviour(gen_server).
-include("erline.hrl").

-export([init/1,
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 terminate/2, 
	 code_change/3]).

-export([start_link/2]).

-record(state, {ref :: reference(),
		next_pipeline :: #pipeline{},
		input :: any()}).

start_link(Pipeline, Input) ->
    gen_server:start_link(?MODULE, [Pipeline, Input], []).

init([Ref, #pipeline{nextline=Nextline}=Pipeline, Input]) ->
    {ok, #state{ref=Ref,
		next_pipeline=Nextline}}.

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
