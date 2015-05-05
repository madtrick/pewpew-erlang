-module(avioneta_game_state_data).

-export([new/1]).
-export([avioneta_game_context_data/1, avioneta_arena_component/1]).
-export([update/2]).

-record(avioneta_game_state_data,{
    avioneta_arena_component,
    avioneta_game_context_data
  }).

new(Options) ->
  #avioneta_game_state_data{
    avioneta_arena_component = proplists:get_value(avioneta_arena_component, Options),
    avioneta_game_context_data = proplists:get_value(avioneta_game_context_data, Options)
  }.

avioneta_arena_component(#avioneta_game_state_data{ avioneta_arena_component = AvionetaArenaComponent }) -> AvionetaArenaComponent.
avioneta_game_context_data(#avioneta_game_state_data{ avioneta_game_context_data = AvionetaGameContextData }) -> AvionetaGameContextData.

update(AvionetaGameStateData, Options) ->
  AvionetaGameStateData#avioneta_game_state_data{
    avioneta_arena_component = proplists:get_value(avioneta_arena_component, Options, avioneta_arena_component(AvionetaGameStateData))
  }.
