-module(avioneta_move_player_command).

-export([fromJSON/1, run/2]).

% {player : , direction: }
fromJSON(JSON) ->
  {[{<<"player">>, Id}, {<<"direction">>, Direction}]} = JSON,
  avioneta_move_player_command_data:new(?MODULE, [{id, Id}, {direction, Direction}]).

run(CommandData, ContextData) ->
  lager:debug("Running move_player_command"),
  Player = avioneta_arena_component:get_player(arena_component(ContextData), player_id(CommandData)),
  avioneta_player_component:move(Player, [{direction, direction(CommandData)}]),
  Player.

arena_component(ContextData) ->
  avioneta_game:arena_component(
    avioneta_command_context_data:avioneta_game(ContextData)
  ).

player_id(CommandData) ->
  avioneta_move_player_command_data:id(CommandData).

direction(CommandData) ->
  avioneta_move_player_command_data:direction(CommandData).
