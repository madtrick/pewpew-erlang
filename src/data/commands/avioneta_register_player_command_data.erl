-module(avioneta_register_player_command_data).

-export([new/2]).

-record(avioneta_register_player_command_data, {}).

new(Runner, _Data) ->
  CommandData = #avioneta_register_player_command_data{},
  avioneta_command_data:new(Runner, CommandData).
