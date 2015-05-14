-module(pewpew_register_player_ack).

-export([new/2, toJSON/1]).

new(_PlayerComponent, Channel) ->
  pewpew_register_player_ack_data:new(?MODULE, Channel, []).

toJSON(AckData) ->
  pewpew_register_player_ack_serializer:toJSON(AckData).
