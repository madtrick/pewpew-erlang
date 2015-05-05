-module(avioneta_destroy_player_command).

-export([fromJSON/1, run/2]).

% {id : }
fromJSON(JSON) ->
  {[{<<"player">>, Id}]} = JSON,
  avioneta_destroy_player_command_data:new(?MODULE, [{id, Id}]).

run(CommandData, ContextData) ->
  lager:debug("Running destroy_player_command"),
  Player = avioneta_arena_component:get_player(arena_component(ContextData), player_id(CommandData)),
  avioneta_player_component:destroy(Player),
  player_id(CommandData).

arena_component(ContextData) ->
  avioneta_game:arena_component(
    avioneta_command_context_data:avioneta_game(ContextData)
  ).

player_id(CommandData) ->
  avioneta_destroy_player_command_data:id(CommandData).
