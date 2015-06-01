-module(pewpew_player_component_snapshot).

-export([
  new/1
]).

new(PlayerComponentData) ->
  X        = pewpew_player_component_data:x(PlayerComponentData),
  Y        = pewpew_player_component_data:y(PlayerComponentData),
  Rotation = pewpew_player_component_data:rotation(PlayerComponentData),
  Life     = pewpew_player_component_data:life(PlayerComponentData),

  #{

     coordinates => #{ x => X, y => Y },
     rotation => Rotation,
     life => Life
  }.
