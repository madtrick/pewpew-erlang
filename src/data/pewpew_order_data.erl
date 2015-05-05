-module(pewpew_order_data).

-export([new/2, order/1, order_data/1]).

-record(pewpew_order_data, {
    order,
    order_data
  }).

new(Order, OrderData) ->
  #pewpew_order_data{
    order = Order,
    order_data = OrderData
  }.

order(#pewpew_order_data{ order = Order }) -> Order.
order_data(#pewpew_order_data{ order_data = OrderData }) -> OrderData.
