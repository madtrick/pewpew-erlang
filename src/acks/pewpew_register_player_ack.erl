-module(pewpew_register_player_ack).

-export([new/1]).

new(PlayerComponent) ->
  Data = pewpew_dataset:new([{player_component, PlayerComponent}]),
  pewpew_message:new(pewpew_register_player_ack_serializer, Data).
