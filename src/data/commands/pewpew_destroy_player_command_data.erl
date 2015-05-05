-module(avioneta_destroy_player_command_data).

-export([new/2]).
-export([id/1]).

-record(avioneta_destroy_player_command_data, {
    id
  }).

new(Runner, Data) ->
  CommandData = #avioneta_destroy_player_command_data{
    id = proplists:get_value(id, Data)
  },

  avioneta_command_data:new(Runner, CommandData).

id(#avioneta_destroy_player_command_data{ id = Id }) -> Id.
