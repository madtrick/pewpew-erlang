-module(pewpew_move_player_ack).

-export([new/2]).

new(Coordinates, Rotation) ->
  Data = pewpew_dataset:new([
    {coordinates, Coordinates},
    {rotation, Rotation}
  ]),
  pewpew_message:new(pewpew_move_player_ack_serializer, Data).
