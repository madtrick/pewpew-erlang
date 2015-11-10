-module(pewpew_start_game_order).

-export([new/0]).

new() ->
  pewpew_message:new(pewpew_start_game_order_serializer).
