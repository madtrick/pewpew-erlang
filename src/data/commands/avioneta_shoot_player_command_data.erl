-module(avioneta_shoot_player_command_data).

-export([new/2]).
-export([id/1, x/1, y/1]).

-record(avioneta_shoot_player_command_data, {
    id,
    x,
    y
  }).

new(Runner, Data) ->
  CommandData = #avioneta_shoot_player_command_data{
    id       = proplists:get_value(id, Data),
    x        = proplists:get_value(x, Data),
    y        = proplists:get_value(y, Data)
  },

  avioneta_command_data:new(Runner, CommandData).

id(#avioneta_shoot_player_command_data{ id = Id }) -> Id.
x(#avioneta_shoot_player_command_data{ x = X }) -> X.
y(#avioneta_shoot_player_command_data{ y = Y }) -> Y.
