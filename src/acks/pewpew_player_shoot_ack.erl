-module(pewpew_player_shoot_ack).

-export([new/1, toJSON/1]).

new(Channel) ->
  Data = [{channel, Channel}],
  pewpew_player_shoot_ack_data:new(?MODULE, Data).

toJSON(AckData) ->
  pewpew_player_shoot_ack_serializer:toJSON(AckData).
