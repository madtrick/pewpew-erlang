-module(pewpew_game_snapshot).

-export([
  new/1
]).

new(GameData) ->
  Arena = pewpew_game_state_data:pewpew_arena_component(GameData),
  ArenaSnapshot = pewpew_arena_component:snapshot(Arena),

  #{
    arena_snapshot => ArenaSnapshot
  }.
