-module(pewpew_start_game_ack).

-export([new/0]).

new() ->
  pewpew_message:new(pewpew_start_game_ack_serializer).
