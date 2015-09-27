-module(pewpew_shot_component_mod).
-include_lib("eunit/include/eunit.hrl").

-export([
  get_coordinates/1,
  update/2,
  move/1,
  snapshot/1
]).

-define(SPEED, 1.5).

get_coordinates(ShotComponentData) ->
  X = pewpew_shot_component_data:x(ShotComponentData),
  Y = pewpew_shot_component_data:y(ShotComponentData),

  {ok, {x, X, y, Y}}.

move(ShotComponentData) ->
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

update(ShotComponentData, UpdateContext) ->
  #{ arena_dimensions := ArenaDimensions, players := Players} = UpdateContext,
  X = pewpew_shot_component_data:x(ShotComponentData),
  Y = pewpew_shot_component_data:y(ShotComponentData),

  case is_out_of_bounds(X, Y, ArenaDimensions) of
    true ->
      {destroy, ShotComponentData};
    false ->
      ShotCircleDescriptor = {x, X, y, Y, radius, 1},
      HitsPlayer = lists:any(fun(Player) ->
        pewpew_utils:circles_intersect(Player, ShotCircleDescriptor)
       end, Players),
      case HitsPlayer of
        true ->
          {destroy, ShotComponentData};
        false ->
          {ok, ShotComponentData}
      end
  end.

snapshot(ShotComponentData) ->
  Snapshot = pewpew_shot_component_snapshot:new(ShotComponentData),
  {ok, Snapshot}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
round_value(Value, Precision) ->
  P = math:pow(10, Precision),
  round(Value * P) / P.

is_out_of_bounds(X, Y, ArenaDimensions) ->
  #{ width := ArenaWidth, height := ArenaHeight } = ArenaDimensions,

  Y < 0 orelse
  X < 0 orelse
  X > ArenaWidth orelse
  Y > ArenaHeight.
