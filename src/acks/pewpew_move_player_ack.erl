-module(pewpew_move_player_ack).

-export([new/2, toJSON/1]).

new(Coordinates, Channel) ->
  Values = [
    {coordinates, Coordinates}
  ],
  pewpew_move_player_ack_data:new(?MODULE, Channel, Values).

toJSON(AckData) ->
  pewpew_move_player_ack_serializer:toJSON(AckData).
