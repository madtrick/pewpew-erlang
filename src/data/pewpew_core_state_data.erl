-module(pewpew_core_state_data).

-export([new/1, pewpew_game/1]).

-record(pewpew_core_state_data, {
    pewpew_game
  }).

new(PewpewGame) ->
  #pewpew_core_state_data{
    pewpew_game = PewpewGame
  }.

pewpew_game(#pewpew_core_state_data{ pewpew_game = PewpewGame }) ->
  PewpewGame.
