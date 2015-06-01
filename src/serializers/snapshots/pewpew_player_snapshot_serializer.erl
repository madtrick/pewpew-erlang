-module(pewpew_player_snapshot_serializer).

-export([to_json/1]).

to_json(Snapshot) ->
  jiffy:encode(Snapshot).
