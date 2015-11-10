-module(pewpew_command_data).

-export([
  new/2,
  command_module/1,
  command_payload/1
]).

new(CommandModule, CommandPayload) ->
  PayloadDataset = pewpew_dataset:new(CommandPayload),
  pewpew_dataset:new([
      {command_module, CommandModule},
      {command_payload, PayloadDataset}
  ]).

command_module(Data) -> pewpew_dataset:get(command_module, Data).
command_payload(Data) -> pewpew_dataset:get(command_payload, Data).
