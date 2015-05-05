-module(pewpew_rotate_player_order_data).

-export([new/2]).
-export([pewpew_player_component/1]).

-record(pewpew_rotate_player_order_data, {
    pewpew_player_component
  }).

new(Order, Data) ->
  OrderData = #pewpew_rotate_player_order_data{
    pewpew_player_component = proplists:get_value(pewpew_player_component, Data)
  },

  pewpew_message_data:new(Order, OrderData).

pewpew_player_component(#pewpew_rotate_player_order_data{ pewpew_player_component = AvionetPlayerComponent }) -> AvionetPlayerComponent.
