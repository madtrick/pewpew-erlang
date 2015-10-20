-module(pewpew_move_player_ack).

-export([new/3, toJSON/1]).

new(Coordinates, Rotation, Channel) ->
  Values = [
    {coordinates, Coordinates},
    {rotation, Rotation}
  ],
  pewpew_move_player_ack_data:new(?MODULE, Channel, Values).

toJSON(AckData) ->
  pewpew_move_player_ack_serializer:toJSON(AckData).
