-module(pewpew_register_player_ack_serializer).

-export([toJSON/1]).

toJSON(RegisterPlayerAckData) ->
  PlayerComponent = pewpew_register_player_ack_data:player_component(RegisterPlayerAckData),
  Struct = {[
      {type, <<"RegisterPlayerAck">>},
      {data, {[
             {id, pewpew_player_component:id(PlayerComponent)},
             {x, pewpew_player_component:x(PlayerComponent)},
             {y, pewpew_player_component:y(PlayerComponent)},
             {life, pewpew_player_component:life(PlayerComponent)}
            ]}}
    ]},
  jiffy:encode(Struct).
