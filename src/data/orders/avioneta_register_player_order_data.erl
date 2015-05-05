-module(avioneta_register_player_order_data).

-export([new/2]).
-export([avioneta_player_component/1, remote/1]).

-record(avioneta_register_player_order_data, {
    remote,
    avioneta_player_component
  }).

new(Order, Data) ->
  OrderData = #avioneta_register_player_order_data{
    remote = proplists:get_value(remote, Data),
    avioneta_player_component = proplists:get_value(avioneta_player_component, Data)
  },

  avioneta_message_data:new(Order, OrderData).

remote(#avioneta_register_player_order_data{ remote = Remote }) -> Remote.
avioneta_player_component(#avioneta_register_player_order_data{ avioneta_player_component = PlayerComponent }) -> PlayerComponent.
