-module(pewpew_shot_hit_player_command).

-export([fromJSON/1, run/2]).

% {player : {}}
fromJSON(JSON) ->
  {[{<<"player">>, PlayerId}, {<<"shot">>, ShotId}]} = JSON,
  pewpew_shot_hit_player_command_data:new(?MODULE, [{player_id, PlayerId}, {shot_id, ShotId}]).

run(CommandData, ContextData) ->
  lager:debug("Running shot hit player command"),
  Player = pewpew_arena_component:get_player(arena_component(ContextData), player_id(CommandData)),
  pewpew_player_component:hit(Player),
  Player.

arena_component(ContextData) ->
  pewpew_game:arena_component(
    pewpew_command_context_data:pewpew_game(ContextData)
  ).

player_id(CommandData) ->
  pewpew_shot_hit_player_command_data:player_id(CommandData).
