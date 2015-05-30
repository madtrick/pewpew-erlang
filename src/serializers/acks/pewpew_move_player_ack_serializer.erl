-module(pewpew_move_player_ack_serializer).

-export([toJSON/1]).


toJSON(AckData) ->
  {x, X, y, Y} = pewpew_move_player_ack_data:coordinates(AckData),
  Struct = {[
    {type, <<"MovePlayerAck">>},
    {data, {[{x, X}, {y, Y}]}}
  ]},
  jiffy:encode(Struct).
