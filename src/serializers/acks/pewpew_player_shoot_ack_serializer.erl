-module(pewpew_player_shoot_ack_serializer).

-export([toJSON/1]).

toJSON(_AckData) ->
  %Struct = #{
  %  type => <<"PlayerShootAck">>,
  %  data => #{}
  % },
  Struct = {[
      {type, <<"PlayerShootAck">>},
      {data, {[]}}
    ]},

  Struct.
