-module(pewpew_configure_player_ack).

-export([new/1, toJSON/1]).

new(Channel) ->
  pewpew_configure_player_ack_data:new(?MODULE, Channel).

toJSON(AckData) ->
  pewpew_configure_player_ack_serializer:toJSON(AckData).
