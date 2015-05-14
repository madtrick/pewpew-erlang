-module(pewpew_register_player_ack_serializer).

-export([toJSON/1]).

toJSON(_RegisterPlayerAckData) ->
  %PlayerComponent = pewpew_move_player_order_data:pewpew_player_component(MovePlayerOrderData),
  Struct = {[
      {type, <<"RegisterPlayerAck">>},
      {data, {[]}}
    ]},
  jiffy:encode(Struct).
