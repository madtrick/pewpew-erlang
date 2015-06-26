-module(pewpew_start_game_ack_serializer).

-export([toJSON/1]).

toJSON(_AckData) ->
  Struct = {[
      {type, <<"StartGameAck">>},
      {data, {[]}}
    ]},
  Struct.
