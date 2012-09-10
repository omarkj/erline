-module(erline_manager_sup).
-behaviour(supervisor).
-include("erline.hrl").
%% API
-export([start_link/0,
	 start_pipeline/3]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, temporary, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_pipeline(Ref, #pipeline{}=Pipe, Input) ->
    Ref = erlang:make_ref(),
    supervisor:start_child(?MODULE, [Ref, Pipe, Input]),
    Ref.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {simple_one_for_one, 0, 1},
	   [?CHILD(erline_line_manager, worker)]} }.
