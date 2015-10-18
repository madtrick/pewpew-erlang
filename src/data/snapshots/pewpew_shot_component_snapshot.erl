-module(pewpew_shot_component_snapshot).

-export([new/1]).

new(ShotComponentData) ->
  X = pewpew_shot_component_data:x(ShotComponentData),
  Y = pewpew_shot_component_data:y(ShotComponentData),
  Id = pewpew_shot_component_data:id(ShotComponentData),

  #{
    coordinates => #{x => X, y => Y},
    id => Id
    }.
