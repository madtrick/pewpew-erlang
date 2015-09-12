-module(pewpew_player_shoot_command).

-export([
  fromJSON/1,
  run/2,
  is_valid/1
]).

% {}
fromJSON(_JSON) ->
  pewpew_player_shoot_command_data:new(?MODULE, []).

is_valid(_) ->
  true.

run(_CommandData, ContextData) ->
  lager:debug("Running player_shoot_command"),

  PewPewGame = pewpew_command_context_data:pewpew_game(ContextData),
  ArenaComponent = pewpew_game:arena_component(PewPewGame),
  Origin = pewpew_command_context_data:origin(ContextData),
  Player = pewpew_arena_component:get_player(ArenaComponent, Origin),
  {ok, _Shot} = pewpew_arena_component:create_shot(ArenaComponent, [{player, Player}]).
