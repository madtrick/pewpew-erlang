-module(pewpew_radar_scan_result_data).

-export([new/1]).
-export([
  scanned_players/1,
  scanned_walls/1
  ]).

new(Options) ->
  pewpew_map_backed_data:new(Options).

scanned_players(#{scanned_players := Value}) -> Value.
scanned_walls(#{scanned_walls := Value}) -> Value.
