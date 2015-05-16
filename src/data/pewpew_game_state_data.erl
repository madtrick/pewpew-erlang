-module(pewpew_game_state_data).

-export([new/1]).
-export([
  pewpew_game_context_data/1,
  pewpew_arena_component/1,
  pewpew_game_status/1
]).
-export([update/2]).

new(Options) ->
  UpdatedOptions = [{pewpew_game_status, not_started} | Options],
  pewpew_map_backed_data:new(UpdatedOptions).

pewpew_arena_component(#{pewpew_arena_component := Value}) -> Value.
pewpew_game_context_data(#{pewpew_game_context_data := Value }) -> Value.
pewpew_game_status(#{pewpew_game_status := Value}) -> Value.

update(PewPewGameStateData, Options) ->
  pewpew_map_backed_data:update(PewPewGameStateData, Options).
