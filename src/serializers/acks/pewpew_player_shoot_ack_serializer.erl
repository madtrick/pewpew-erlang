-module(pewpew_player_shoot_ack_serializer).

-export([toJSON/1]).

toJSON(AckData) ->
  ShootingInfo = pewpew_dataset:get(shooting_info, AckData),
  Struct = #{
    type => <<"PlayerShootAck">>,
      data => #{shooting => ShootingInfo}
   },

  Struct.
