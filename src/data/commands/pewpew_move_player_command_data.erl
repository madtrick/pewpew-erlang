-module(pewpew_move_player_command_data).

-export([new/2]).
-export([movements/1]).

new(CommandModule, Data) ->
  CommandData = pewpew_map_backed_data:new(Data),
  pewpew_command_data:new(CommandModule, CommandData).

movements(#{movements := Value}) -> Value.
