-module(pewpew_start_game_order).

-export([
  new/0,
  toJSON/1
]).

new() ->
  pewpew_message_data:new(?MODULE, []).

toJSON(OrderData) ->
  pewpew_start_game_order_serializer:toJSON(OrderData).
