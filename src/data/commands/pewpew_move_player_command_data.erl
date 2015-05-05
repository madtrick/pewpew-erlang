-module(pewpew_move_player_command_data).

-export([new/2]).
-export([id/1, direction/1]).

-record(pewpew_move_player_command_data, {
    id,
    direction
  }).

new(Runner, Data) ->
  CommandData = #pewpew_move_player_command_data{
    id = proplists:get_value(id, Data),
    direction = proplists:get_value(direction, Data)
  },

  pewpew_command_data:new(Runner, CommandData).

id(#pewpew_move_player_command_data{ id = Id }) -> Id.
direction(#pewpew_move_player_command_data{ direction = Direction }) -> Direction.
