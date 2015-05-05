-module(pewpew_move_player_order_serializer).

-export([toJSON/1]).

toJSON(MovePlayerOrderData) ->
  PlayerComponent = pewpew_move_player_order_data:pewpew_player_component(MovePlayerOrderData),
  Struct = {[
      {type, <<"MovePlayerOrder">>},
      {data, {[
            {id, pewpew_player_component:id(PlayerComponent)},
            {direction, pewpew_move_player_order_data:direction(MovePlayerOrderData)}
          ]}
      }
    ]},
  jiffy:encode(Struct).
