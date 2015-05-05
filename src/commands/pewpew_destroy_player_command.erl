-module(pewpew_destroy_player_command).

-export([fromJSON/1, run/2]).

% {id : }
fromJSON(JSON) ->
  {[{<<"player">>, Id}]} = JSON,
  pewpew_destroy_player_command_data:new(?MODULE, [{id, Id}]).

run(CommandData, ContextData) ->
  lager:debug("Running destroy_player_command"),
  Player = pewpew_arena_component:get_player(arena_component(ContextData), player_id(CommandData)),
  pewpew_player_component:destroy(Player),
  player_id(CommandData).

arena_component(ContextData) ->
  pewpew_game:arena_component(
    pewpew_command_context_data:pewpew_game(ContextData)
  ).

player_id(CommandData) ->
  pewpew_destroy_player_command_data:id(CommandData).
