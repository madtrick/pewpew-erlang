-module(pewpew_configure_player_ack_serializer).

-export([toJSON/1]).


toJSON(_) ->
  Struct = #{
    type => <<"ConfigurePlayerAck">>,
    data => #{}
  },

  Struct.
