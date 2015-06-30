-module(pewpew_shoot_player_context).

-export([call/3]).

call(CommandContextData, PewpewGame, OriginChannel) ->
  UpdatedCommandContextData = pewpew_command_context_data:update(CommandContextData, [{origin, OriginChannel}, {pewpew_game, PewpewGame}]),
  CommandData = pewpew_command_context_data:command_data(UpdatedCommandContextData),

  Shot = (pewpew_command_data:runner(CommandData)):run(
    pewpew_command_data:runner_data(CommandData), UpdatedCommandContextData
  ),

  ShootPlayerOrder = pewpew_shoot_player_order:new([{shot, Shot}]),

  {reply, [{send_to_all, [ShootPlayerOrder]}]}.
