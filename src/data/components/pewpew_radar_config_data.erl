-module(pewpew_radar_config_data).

-export([new/1]).
-export([mode/1, radius/1]).

new(Options) ->
  pewpew_map_backed_data:new(Options).

mode(#{mode := Value}) -> Value.
radius(#{radius := Value}) -> Value.
