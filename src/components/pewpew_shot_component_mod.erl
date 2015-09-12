-module(pewpew_shot_component_mod).

-export([
  get_coordinates/1,
  update/1
]).

-define(SPEED, 1.5).

get_coordinates(ShotComponentData) ->
  X = pewpew_shot_component_data:x(ShotComponentData),
  Y = pewpew_shot_component_data:y(ShotComponentData),

  {ok, {x, X, y, Y}}.

update(ShotComponentData) ->
  Rotation = pewpew_shot_component_data:rotation(ShotComponentData),
  RadianRotation = (Rotation * math:pi()) / 180,
  DX       = ?SPEED * math:cos(RadianRotation),
  DY       = ?SPEED * math:sin(RadianRotation),
  X        = pewpew_shot_component_data:x(ShotComponentData),
  Y        = pewpew_shot_component_data:y(ShotComponentData),

  RoundedX = round_value(X + DX, 5),
  RoundedY = round_value(Y + DY, 5),

  UpdatedShotComponentData = pewpew_shot_component_data:update(ShotComponentData, [{x, RoundedX}, {y, RoundedY}]),

  {ok, UpdatedShotComponentData}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
round_value(Value, Precision) ->
  P = math:pow(10, Precision),
  round(Value * P) / P.
