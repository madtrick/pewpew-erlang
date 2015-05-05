-module(pewpew_shoot_player_command_data).

-export([new/2]).
-export([id/1, x/1, y/1]).

-record(pewpew_shoot_player_command_data, {
    id,
    x,
    y
  }).

new(Runner, Data) ->
  CommandData = #pewpew_shoot_player_command_data{
    id       = proplists:get_value(id, Data),
    x        = proplists:get_value(x, Data),
    y        = proplists:get_value(y, Data)
  },

  pewpew_command_data:new(Runner, CommandData).

id(#pewpew_shoot_player_command_data{ id = Id }) -> Id.
x(#pewpew_shoot_player_command_data{ x = X }) -> X.
y(#pewpew_shoot_player_command_data{ y = Y }) -> Y.
