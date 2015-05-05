-module(avioneta_shoot_player_order_serializer).

-export([toJSON/1]).

toJSON(ShootPlayerOrderData) ->
  Shot = avioneta_shoot_player_order_data:shot(ShootPlayerOrderData),
  Struct = {[
      {type, <<"ShootPlayerOrder">>},
      {data, {[
            {id, avioneta_shot_data:id(Shot)},
            {shot_id, avioneta_shot_data:shot_id(Shot)}
          ]}}
    ]},
  jiffy:encode(Struct).
