-module(pewpew_radar_data).

-export([new/1]).
-export([mode/1, radius/1]).

new(Options) ->
  pewpew_map_backed_data:new(Options).

mode(#{radar_mode := Value}) -> Value.
radius(#{rada_radius := Value}) -> Value.
