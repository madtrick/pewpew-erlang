-module(pewpew_move_player_ack_serializer).

-export([toJSON/1]).


toJSON(AckData) ->
  {x, X, y, Y} = pewpew_move_player_ack_data:coordinates(AckData),
  Rotation     = pewpew_move_player_ack_data:rotation(AckData),

  Struct = {[
    {type, <<"MovePlayerAck">>},
    {data, {[{x, X}, {y, Y}, {rotation, Rotation}]}}
  ]},

  Struct.
