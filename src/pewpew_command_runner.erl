-module(pewpew_command_runner).
-export([run/1]).

run(CommandContext) ->
  call_context(CommandContext).

call_context(CommandContext) ->
  (pewpew_command_context_data:context(CommandContext)):call(CommandContext).
