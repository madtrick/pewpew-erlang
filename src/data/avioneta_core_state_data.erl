-module(avioneta_core_state_data).

-export([new/1, avioneta_game/1]).

-record(avioneta_core_state_data, {
    avioneta_game
  }).

new(AvionetaGame) ->
  #avioneta_core_state_data{
    avioneta_game = AvionetaGame
  }.

avioneta_game(#avioneta_core_state_data{ avioneta_game = AvionetaGame }) ->
  AvionetaGame.
