-module(pewpew_move_player_command).

-export([fromJSON/1, run/2]).

% {player : , direction: }
fromJSON(JSON) ->
  {[{<<"player">>, Id}, {<<"direction">>, Direction}]} = JSON,
  pewpew_move_player_command_data:new(?MODULE, [{id, Id}, {direction, Direction}]).

run(CommandData, ContextData) ->
  lager:debug("Running move_player_command"),
  Player = pewpew_arena_component:get_player(arena_component(ContextData), player_id(CommandData)),
  pewpew_player_component:move(Player, [{direction, direction(CommandData)}]),
  Player.

arena_component(ContextData) ->
  pewpew_game:arena_component(
    pewpew_command_context_data:pewpew_game(ContextData)
  ).

player_id(CommandData) ->
  pewpew_move_player_command_data:id(CommandData).

direction(CommandData) ->
  pewpew_move_player_command_data:direction(CommandData).
