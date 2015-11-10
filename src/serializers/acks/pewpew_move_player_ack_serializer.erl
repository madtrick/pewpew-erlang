-module(pewpew_move_player_ack_serializer).
-include_lib("eunit/include/eunit.hrl").

-export([toJSON/1]).


toJSON(AckData) ->
  {x, X, y, Y} = pewpew_dataset:get(coordinates, AckData),
  Rotation     = pewpew_dataset:get(rotation, AckData),

  Struct = {[
    {type, <<"MovePlayerAck">>},
    {data, {[{x, X}, {y, Y}, {rotation, Rotation}]}}
  ]},

  Struct.
