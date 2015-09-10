-module(pewpew_player_shoot_ack_data).

-export([new/2]).

new(AckModule, Data) ->
  Channel = proplists:get_value(channel, Data),
  pewpew_message_data:new(AckModule, Channel, undefined).
