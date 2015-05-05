-module(pewpew_rotate_player_order_serializer).

-export([toJSON/1]).

toJSON(RotatePlayerOrderData) ->
  PlayerComponent = pewpew_rotate_player_order_data:pewpew_player_component(RotatePlayerOrderData),
  Struct = {[
      {type, <<"RotatePlayerOrder">>},
      {data, {[
            {id, pewpew_player_component:id(PlayerComponent)},
            {rotation, pewpew_player_component:rotation(PlayerComponent)}
          ]}
      }
    ]},
  jiffy:encode(Struct).
