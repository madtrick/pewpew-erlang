-module(pewpew_start_game_order_serializer).

-export([toJSON/1]).

toJSON(_OrderData) ->
  Struct ={[
    {type, <<"StartGameOrder">>},
    {data, {[]}}
  ]},

  Struct.
