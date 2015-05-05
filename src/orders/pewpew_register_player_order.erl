-module(pewpew_register_player_order).

-export([new/2, toJSON/1]).

new(PlayerComponent, Data) ->
  pewpew_register_player_order_data:new(?MODULE, [{pewpew_player_component, PlayerComponent} | Data ]).

toJSON(OrderData) ->
  pewpew_register_player_order_serializer:toJSON(OrderData).



