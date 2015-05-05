-module(avioneta_shoot_player_order).

-export([new/1, toJSON/1]).

new(Data) ->
  avioneta_shoot_player_order_data:new(?MODULE, Data).

toJSON(OrderData) ->
  avioneta_shoot_player_order_serializer:toJSON(OrderData).
