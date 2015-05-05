-module(pewpew_move_player_order_data).

-export([new/2]).
-export([pewpew_player_component/1, direction/1]).

-record(pewpew_move_player_order_data, {
    direction,
    pewpew_player_component
  }).

new(Order, Data) ->
  OrderData = #pewpew_move_player_order_data{
    direction = proplists:get_value(direction, Data),
    pewpew_player_component = proplists:get_value(pewpew_player_component, Data)
  },

  pewpew_message_data:new(Order, OrderData).

pewpew_player_component(#pewpew_move_player_order_data{ pewpew_player_component = AvionetPlayerComponent }) -> AvionetPlayerComponent.
direction(#pewpew_move_player_order_data{ direction = Direction }) -> Direction.
