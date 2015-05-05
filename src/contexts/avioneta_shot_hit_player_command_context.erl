-module(avioneta_shot_hit_player_command_context).

-export([call/3]).

call(CommandContextData, AvionetaGame, OriginChannel) ->
  UpdatedCommandContextData = avioneta_command_context_data:update(CommandContextData, [{origin, OriginChannel}, {avioneta_game, AvionetaGame}]),
  CommandData = avioneta_command_context_data:command_data(UpdatedCommandContextData),

  _ = (avioneta_command_data:runner(CommandData)):run(
    avioneta_command_data:runner_data(CommandData), UpdatedCommandContextData
  ),

  noreply.
