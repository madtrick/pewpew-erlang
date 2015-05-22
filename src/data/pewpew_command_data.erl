-module(pewpew_command_data).

-export([new/2, command_module/1, command_data/1]).

-record(pewpew_command_data, {
    command_module,
    command_data
  }).

new(CommandModule, Data) ->
  #pewpew_command_data{
    command_module = CommandModule,
    command_data   = Data
  }.

command_module(#pewpew_command_data{ command_module = Module}) -> Module.
command_data(#pewpew_command_data{ command_data = Data}) -> Data.
