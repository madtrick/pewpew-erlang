-module(pewpew_shoot_player_order_data).

-export([new/2]).
-export([shot/1]).

-record(pewpew_shoot_player_order_data, {
    shot
  }).

new(Order, Data) ->
  OrderData = #pewpew_shoot_player_order_data{
    shot = proplists:get_value(shot, Data)
  },

  pewpew_message_data:new(Order, OrderData).

shot(#pewpew_shoot_player_order_data{ shot = Shot }) -> Shot.
