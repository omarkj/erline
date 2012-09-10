-module(erline_action_sup).
-behaviour(supervisor).
-include("erline.hrl").
%% API
-export([start_link/0,
	 start_action/4]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, temporary, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_action(sequential, Action, Opts, Input) ->
    {ok, _Pid} = supervisor:start_child(?MODULE, [self(), Action, Opts, Input]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {simple_one_for_one, 0, 1},
	   [?CHILD(erline_action, worker)]} }.
