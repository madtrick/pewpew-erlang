-module(pewpew_start_game_command_data).

-export([new/2]).

-record(pewpew_start_game_command_data, {}).

new(Runner, _Data) ->
  CommandData = #pewpew_start_game_command_data{},
  pewpew_command_data:new(Runner, CommandData).