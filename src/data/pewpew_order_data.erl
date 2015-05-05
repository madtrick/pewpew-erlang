-module(avioneta_order_data).

-export([new/2, order/1, order_data/1]).

-record(avioneta_order_data, {
    order,
    order_data
  }).

new(Order, OrderData) ->
  #avioneta_order_data{
    order = Order,
    order_data = OrderData
  }.

order(#avioneta_order_data{ order = Order }) -> Order.
order_data(#avioneta_order_data{ order_data = OrderData }) -> OrderData.
