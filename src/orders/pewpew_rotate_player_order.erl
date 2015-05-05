-module(pewpew_rotate_player_order).

-export([new/1, toJSON/1]).

new(PewpewPlayerComponent) ->
  pewpew_rotate_player_order_data:new(?MODULE, [{pewpew_player_component, PewpewPlayerComponent}]).

toJSON(OrderData) ->
  pewpew_rotate_player_order_serializer:toJSON(OrderData).
