-module(erline_action).

-include("erline.hrl").

-export([start_link/4]).
-export([runner/4]).

start_link(Caller, Action, Opts, Input) ->
    Pid = spawn_link(erline_action, runner, [Caller, Action, Opts, Input]),
    {ok, Pid}.

runner(Caller, Actions, Opts, Input) when is_list(Actions) ->
    case lists:keyfind(init_with, 1, Opts) of
	{init_with, InitWith} ->
	    ok = erline_line_manager:action_return(Caller, handle_actions(Actions, InitWith, Input))
    end;
runner(Caller, Actions, _Opts, Input) ->
    runner(Caller, [Actions], _Opts, Input).

handle_actions([], _, Output) ->
    Output;
handle_actions([Action|Actions], InitWith, Input) ->
    case handle_action(Action, create_input(InitWith, Input)) of
	{action_error, _}=Error ->
	    handle_actions([], InitWith, Error);
	Res ->
	    handle_actions(Actions, InitWith, Res)
    end.

handle_action(Action, Input) when is_function(Action) ->
    try Action(Input) of
	R -> R
    catch
	_:_=Error ->
	    {action_error, Error}
    end;
handle_action(Action, Input) when is_atom(Action) ->
    try Action:handle(Input) of
	R -> R
    catch
	_:_=Error ->
	    error_logger:error_msg("Action ~p crashed with error ~p", [Action, Error]),
	    UserError = 
		case erlang:function_exported(Action, on_crash, 1) of
		    true -> catch Action:on_crash(Error);
		    _ -> Error
		end,
	    {action_error, UserError}
    end;
handle_action([#pipeline{}|_]=Actions, Input) ->
    erline:sync(Actions, Input).

create_input(undefined, Input) ->
    Input;
create_input(InitWith, Input) ->
    [InitWith, Input].
