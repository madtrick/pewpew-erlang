-module(pewpew_game_state_data).

-export([new/1]).
-export([
  pewpew_game_context_data/1,
  pewpew_arena_component/1,
  pewpew_game_status/1
]).
-export([update/2]).

new(Options) ->
  PewPewArenaComponent  = proplists:get_value(pewpew_arena_component, Options),
  PewPewGameContextData = proplists:get_value(pewpew_game_context_data, Options),

  #{
    pewpew_arena_component => PewPewArenaComponent,
    pewpew_game_context_data => PewPewGameContextData,
    pewpew_game_status => not_started
   }.

pewpew_arena_component(#{pewpew_arena_component := Value}) -> Value.
pewpew_game_context_data(#{pewpew_game_context_data := Value }) -> Value.
pewpew_game_status(#{pewpew_game_status := Value}) -> Value.

update(PewPewGameStateData, Options) ->
  MapKeys = maps:keys(PewPewGameStateData),

  lists:foldl(fun(Key, Acc) ->
    Default = maps:get(Key, Acc),
    MaybeOptionValue = proplists:get_value(Key, Options, Default),

    maps:update(Key, MaybeOptionValue, Acc)
  end, PewPewGameStateData, MapKeys).
