-module(pewpew_command_runner).

-export([run/3]).

run(CommandContexts, PewpewGame, Origin) ->
  run(CommandContexts, PewpewGame, Origin, []).

run([], PewpewGame, Origin, Orders) ->
  lists:flatten(Orders);
run([CommandContext | CommandContexts], PewpewGame, Origin,  Orders) ->
  NewOrders = call_context(CommandContext, PewpewGame, Origin),
  run(CommandContexts, PewpewGame, Origin, [NewOrders | Orders]).

call_context(CommandContext, PewpewGame, Origin) ->
  (pewpew_command_context_data:context(CommandContext)):call(CommandContext, PewpewGame, Origin).
