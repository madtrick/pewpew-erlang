-module(avioneta_rotate_player_command_data).

-export([new/2]).
-export([id/1, rotation/1]).

-record(avioneta_rotate_player_command_data, {
    id,
    rotation
  }).

new(Runner, Data) ->
  CommandData = #avioneta_rotate_player_command_data{
    id = proplists:get_value(id, Data),
    rotation = proplists:get_value(rotation, Data)
  },

  avioneta_command_data:new(Runner, CommandData).

id(#avioneta_rotate_player_command_data{ id = Id }) -> Id.
rotation(#avioneta_rotate_player_command_data{ rotation = Rotation }) -> Rotation.
