-module(erline_action).

-include("erline.hrl").

-export([start_link/4]).
-export([runner/4]).

start_link(Caller, Action, Opts, Input) ->
    Pid = spawn_link(erline_action, runner, [Caller, Action, Opts, Input]),
    {ok, Pid}.

runner(Caller, Actions, _Opts, Input) ->
    ok = erline_line_manager:action_return(Caller, handle_action(Actions, Input)).

handle_action([], Output) ->
    Output;
handle_action([Action|Actions], Input) when is_record(Action, pipeline) ->
    Res = erline:sync(Action, Input),
    handle_action(Actions, Res);
handle_action([Action|Actions], Input) when is_atom(Action) ->
    handle_action(Actions, Action:handle(Input));
handle_action([Action|Actions], Input) when is_function(Action) ->
    handle_action(Actions, Action(Input)).
