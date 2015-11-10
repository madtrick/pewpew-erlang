-module(pewpew_configure_player_ack).

-export([new/0]).

new() ->
  pewpew_message:new(pewpew_configure_player_ack_serializer).
