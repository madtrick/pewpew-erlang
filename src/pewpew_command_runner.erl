-module(avioneta_command_runner).

-export([run/3]).

run(CommandContexts, AvionetaGame, Origin) ->
  run(CommandContexts, AvionetaGame, Origin, []).

run([], AvionetaGame, Origin, Orders) ->
  lists:flatten(Orders);
run([CommandContext | CommandContexts], AvionetaGame, Origin,  Orders) ->
  NewOrders = call_context(CommandContext, AvionetaGame, Origin),
  run(CommandContexts, AvionetaGame, Origin, [NewOrders | Orders]).

call_context(CommandContext, AvionetaGame, Origin) ->
  (avioneta_command_context_data:context(CommandContext)):call(CommandContext, AvionetaGame, Origin).
