-module(avioneta_shoot_player_order_data).

-export([new/2]).
-export([shot/1]).

-record(avioneta_shoot_player_order_data, {
    shot
  }).

new(Order, Data) ->
  OrderData = #avioneta_shoot_player_order_data{
    shot = proplists:get_value(shot, Data)
  },

  avioneta_message_data:new(Order, OrderData).

shot(#avioneta_shoot_player_order_data{ shot = Shot }) -> Shot.
