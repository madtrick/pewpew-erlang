-module(pewpew_player_shoot_ack_serializer).

-export([toJSON/1]).

toJSON(AckData) ->
  ShootingInfo = pewpew_player_shoot_ack_data:shooting_info(AckData),
  Struct = #{
    type => <<"PlayerShootAck">>,
      data => #{shooting => ShootingInfo}
   },

  Struct.
