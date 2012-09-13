-module(erline_action).

-include("erline.hrl").

-export([start_link/4]).
-export([runner/4]).

start_link(Caller, Action, Opts, Input) ->
    Pid = spawn_link(erline_action, runner, [Caller, Action, Opts, Input]),
    {ok, Pid}.

runner(Caller, Actions, _Opts, Input) when is_list(Actions) ->
    ok = erline_line_manager:action_return(Caller, handle_actions(Actions, Input));
runner(Caller, Actions, _Opts, Input) ->
    runner(Caller, [Actions], _Opts, Input).

handle_actions([], Output) ->
    Output;
handle_actions([Action|Actions], Input) ->
    handle_actions(Actions, handle_action(Action, Input)).

handle_action(Action, Input) when is_function(Action) ->
    Action(Input);
handle_action(Action, Input) when is_atom(Action) ->
    Action:handle(Input);
handle_action([#pipeline{}|_]=Actions, Input) ->
    erline:sync(Actions, Input).



