-module(avioneta_rotate_player_command).

-export([fromJSON/1, run/2]).

% {player : , rotation: }
fromJSON(JSON) ->
  {[{<<"player">>, Id}, {<<"rotation">>, Rotation}]} = JSON,
  avioneta_rotate_player_command_data:new(?MODULE, [{id, Id}, {rotation, Rotation}]).

run(CommandData, ContextData) ->
  lager:debug("Running rotate_player_command"),
  Player = avioneta_arena_component:get_player(arena_component(ContextData), player_id(CommandData)),
  avioneta_player_component:rotate(Player, rotation(CommandData)),
  Player.

arena_component(ContextData) ->
  avioneta_game:arena_component(
    avioneta_command_context_data:avioneta_game(ContextData)
  ).

player_id(CommandData) ->
  avioneta_rotate_player_command_data:id(CommandData).

rotation(CommandData) ->
  avioneta_rotate_player_command_data:rotation(CommandData).
