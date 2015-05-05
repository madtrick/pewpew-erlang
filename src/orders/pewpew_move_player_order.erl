-module(pewpew_move_player_order).

-export([new/2, toJSON/1]).

new(PewpewPlayerComponent, Data) ->
  pewpew_move_player_order_data:new(?MODULE, [{pewpew_player_component, PewpewPlayerComponent} | Data]).

toJSON(OrderData) ->
  pewpew_move_player_order_serializer:toJSON(OrderData).
