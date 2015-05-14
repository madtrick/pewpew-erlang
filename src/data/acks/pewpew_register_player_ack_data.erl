-module(pewpew_register_player_ack_data).

-export([new/3]).

-record(pewpew_register_player_ack_data, {}).

new(Ack, Channel, _Data) ->
  AckData = #pewpew_register_player_ack_data{},
  pewpew_message_data:new(Ack, Channel, AckData).
