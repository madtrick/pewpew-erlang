-module(pewpew_rotate_player_command).

-export([fromJSON/1, run/2]).

% {player : , rotation: }
fromJSON(JSON) ->
  {[{<<"player">>, Id}, {<<"rotation">>, Rotation}]} = JSON,
  pewpew_rotate_player_command_data:new(?MODULE, [{id, Id}, {rotation, Rotation}]).

run(CommandData, ContextData) ->
  lager:debug("Running rotate_player_command"),
  Player = pewpew_arena_component:get_player(arena_component(ContextData), player_id(CommandData)),
  pewpew_player_component:rotate(Player, rotation(CommandData)),
  Player.

arena_component(ContextData) ->
  pewpew_game:arena_component(
    pewpew_command_context_data:pewpew_game(ContextData)
  ).

player_id(CommandData) ->
  pewpew_rotate_player_command_data:id(CommandData).

rotation(CommandData) ->
  pewpew_rotate_player_command_data:rotation(CommandData).
