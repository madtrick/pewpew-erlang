-module(pewpew_destroy_player_command_data).

-export([new/2]).
-export([id/1]).

-record(pewpew_destroy_player_command_data, {
    id
  }).

new(Runner, Data) ->
  CommandData = #pewpew_destroy_player_command_data{
    id = proplists:get_value(id, Data)
  },

  pewpew_command_data:new(Runner, CommandData).

id(#pewpew_destroy_player_command_data{ id = Id }) -> Id.
