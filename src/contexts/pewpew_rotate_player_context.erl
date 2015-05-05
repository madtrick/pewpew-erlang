-module(pewpew_rotate_player_context).

-export([call/3]).

call(CommandContextData, PewpewGame, OriginChannel) ->
  UpdatedCommandContextData = pewpew_command_context_data:update(CommandContextData, [{origin, OriginChannel}, {pewpew_game, PewpewGame}]),
  CommandData = pewpew_command_context_data:command_data(UpdatedCommandContextData),

  Player = (pewpew_command_data:runner(CommandData)):run(
    pewpew_command_data:runner_data(CommandData), UpdatedCommandContextData
  ),

  RotatePlayerOrder = pewpew_rotate_player_order:new(Player),

  {reply, [{send_to_others, [RotatePlayerOrder]}]}.

