-module(pewpew_player_component_snapshot).

-export([
  new/1
]).

new(PlayerComponentData) ->
  Id       = pewpew_player_component_data:id(PlayerComponentData),
  X        = pewpew_player_component_data:x(PlayerComponentData),
  Y        = pewpew_player_component_data:y(PlayerComponentData),
  Rotation = pewpew_player_component_data:rotation(PlayerComponentData),
  Life     = pewpew_player_component_data:life(PlayerComponentData),

  #{
     id => Id,
     coordinates => #{ x => X, y => Y },
     rotation => Rotation,
     life => Life
  }.
