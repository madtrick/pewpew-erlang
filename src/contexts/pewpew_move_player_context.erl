-module(pewpew_move_player_context).

-export([call/3]).

call(CommandContextData, PewpewGame, OriginChannel) ->
  UpdatedCommandContextData = pewpew_command_context_data:update(CommandContextData, [{origin, OriginChannel}, {pewpew_game, PewpewGame}]),
  CommandData = pewpew_command_context_data:command_data(UpdatedCommandContextData),

  Player = (pewpew_command_data:runner(CommandData)):run(
    pewpew_command_data:runner_data(CommandData), UpdatedCommandContextData
  ),

  MovePlayerOrder = pewpew_move_player_order:new(Player, [{direction, pewpew_move_player_command_data:direction(pewpew_command_data:runner_data(CommandData))}]),

  {reply, [{send_to_others, [MovePlayerOrder]}]}.

