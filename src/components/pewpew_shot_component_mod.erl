-module(pewpew_shot_component_mod).

-export([get_coordinates/1]).

get_coordinates(ShotComponentData) ->
  X = pewpew_shot_component_data:x(ShotComponentData),
  Y = pewpew_shot_component_data:y(ShotComponentData),

  {ok, {x, X, y, Y}}.
