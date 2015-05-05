-module(avioneta_move_player_order).

-export([new/2, toJSON/1]).

new(AvionetaPlayerComponent, Data) ->
  avioneta_move_player_order_data:new(?MODULE, [{avioneta_player_component, AvionetaPlayerComponent} | Data]).

toJSON(OrderData) ->
  avioneta_move_player_order_serializer:toJSON(OrderData).
