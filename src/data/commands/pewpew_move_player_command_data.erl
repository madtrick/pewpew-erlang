-module(avioneta_move_player_command_data).

-export([new/2]).
-export([id/1, direction/1]).

-record(avioneta_move_player_command_data, {
    id,
    direction
  }).

new(Runner, Data) ->
  CommandData = #avioneta_move_player_command_data{
    id = proplists:get_value(id, Data),
    direction = proplists:get_value(direction, Data)
  },

  avioneta_command_data:new(Runner, CommandData).

id(#avioneta_move_player_command_data{ id = Id }) -> Id.
direction(#avioneta_move_player_command_data{ direction = Direction }) -> Direction.
