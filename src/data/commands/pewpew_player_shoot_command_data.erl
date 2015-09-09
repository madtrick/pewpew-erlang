-module(pewpew_player_shoot_command_data).

-export([new/2]).

new(CommandModule, _Data) ->
  pewpew_command_data:new(CommandModule, undefined_pewpew_shoot_player_command_data).
