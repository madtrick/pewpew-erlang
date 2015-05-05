-module(pewpew_game_state_data).

-export([new/1]).
-export([pewpew_game_context_data/1, pewpew_arena_component/1]).
-export([update/2]).

-record(pewpew_game_state_data,{
    pewpew_arena_component,
    pewpew_game_context_data
  }).

new(Options) ->
  #pewpew_game_state_data{
    pewpew_arena_component = proplists:get_value(pewpew_arena_component, Options),
    pewpew_game_context_data = proplists:get_value(pewpew_game_context_data, Options)
  }.

pewpew_arena_component(#pewpew_game_state_data{ pewpew_arena_component = PewpewArenaComponent }) -> PewpewArenaComponent.
pewpew_game_context_data(#pewpew_game_state_data{ pewpew_game_context_data = PewpewGameContextData }) -> PewpewGameContextData.

update(PewpewGameStateData, Options) ->
  PewpewGameStateData#pewpew_game_state_data{
    pewpew_arena_component = proplists:get_value(pewpew_arena_component, Options, pewpew_arena_component(PewpewGameStateData))
  }.
