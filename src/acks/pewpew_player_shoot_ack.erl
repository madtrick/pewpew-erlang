-module(pewpew_player_shoot_ack).

-export([new/1]).

new(ShootingInfo) ->
  Data = pewpew_dataset:new([{shooting_info, ShootingInfo}]),
  pewpew_message:new(pewpew_player_shoot_ack_serializer, Data).
