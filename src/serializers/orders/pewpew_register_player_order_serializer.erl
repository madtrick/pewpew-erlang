-module(pewpew_register_player_order_serializer).

-export([toJSON/1]).

toJSON(RegisterPlayerOrderData) ->
  PlayerComponent = pewpew_register_player_order_data:pewpew_player_component(RegisterPlayerOrderData),
  Struct = {[
          {type, <<"RegisterPlayerOrder">>},
          {data, {[
                {id, pewpew_player_component:id(PlayerComponent)},
                {x, pewpew_player_component:x(PlayerComponent)},
                {y, pewpew_player_component:y(PlayerComponent)},
                {color, pewpew_player_component:color(PlayerComponent)},
                {name, pewpew_player_component:name(PlayerComponent)},
                {life, pewpew_player_component:life(PlayerComponent)},
                {remote, pewpew_register_player_order_data:remote(RegisterPlayerOrderData)}
              ]}
          }
        ]},
  jiffy:encode(Struct).

