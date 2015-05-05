-module(avioneta_rotate_player_order_serializer).

-export([toJSON/1]).

toJSON(RotatePlayerOrderData) ->
  PlayerComponent = avioneta_rotate_player_order_data:avioneta_player_component(RotatePlayerOrderData),
  Struct = {[
      {type, <<"RotatePlayerOrder">>},
      {data, {[
            {id, avioneta_player_component:id(PlayerComponent)},
            {rotation, avioneta_player_component:rotation(PlayerComponent)}
          ]}
      }
    ]},
  jiffy:encode(Struct).
