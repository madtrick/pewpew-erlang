-module(pewpew_configure_player_ack_data).

-export([new/2]).

new(AckModule, Channel) ->
  pewpew_message_data:new(AckModule, Channel, undefined).
