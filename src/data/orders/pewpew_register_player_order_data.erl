-module(pewpew_register_player_order_data).

-export([new/2]).
-export([pewpew_player_component/1, remote/1]).

-record(pewpew_register_player_order_data, {
    remote,
    pewpew_player_component
  }).

new(Order, Data) ->
  OrderData = #pewpew_register_player_order_data{
    remote = proplists:get_value(remote, Data),
    pewpew_player_component = proplists:get_value(pewpew_player_component, Data)
  },

  pewpew_message_data:new(Order, OrderData).

remote(#pewpew_register_player_order_data{ remote = Remote }) -> Remote.
pewpew_player_component(#pewpew_register_player_order_data{ pewpew_player_component = PlayerComponent }) -> PlayerComponent.
