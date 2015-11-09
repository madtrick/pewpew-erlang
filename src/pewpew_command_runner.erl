-module(pewpew_command_runner).
-include_lib("eunit/include/eunit.hrl").
-export([run/1]).

run(CommandContextData) ->
  call_context(CommandContextData).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
call_context(CommandContextData) ->
  %(pewpew_command_context_data:context(CommandContextData)):call(CommandContextData).
  Context = pewpew_command_context_data:context(CommandContextData),
  ensure_context_module_loaded(Context),
  case erlang:function_exported(Context, skip_middleware, 0) of
    true ->
      MiddlewareToRun = lists:subtract(middleware(), Context:skip_middleware()),
      apply_middleware(MiddlewareToRun, CommandContextData);
    false ->
      apply_middleware(middleware(), CommandContextData)
  end.

middleware() ->
  [
    pewpew_middleware_is_valid_command,
    pewpew_middleware_is_player_registered,
    pewpew_middleware_is_game_started
    ].

apply_middleware([], CommandContextData) ->
  Context = pewpew_command_context_data:context(CommandContextData),
  Context:call(CommandContextData);
apply_middleware([Middleware | Tail], CommandContextData) ->
  F = fun (U) ->
    apply_middleware(Tail, U)
  end,

  Middleware:run(CommandContextData, F).

% Module needs to be loaded before using the
% the erlang:function_exportend function
ensure_context_module_loaded(Module) ->
  maybe_load_module(Module, code:is_loaded(Module)).

maybe_load_module(_, {file, _}) ->
  ok;
maybe_load_module(Module, false) ->
  {module, _} = code:load_file(Module),
  ok.
