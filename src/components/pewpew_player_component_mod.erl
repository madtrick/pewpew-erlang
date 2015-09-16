-module(pewpew_player_component_mod).
-include_lib("eunit/include/eunit.hrl").

-export([
  set_coordinates/2,
  get_coordinates/1,
  move/2,
  snapshot/1,
  update/2,
  configure/3
]).

-define(MOVEMENT_SPEED, 1).
% TODO remove the RADAR_MODES constant
-define(RADAR_MODES, [<<"long_range_scan">>]).
-define(LIFE_LOSS_BY_SHOT_HIT, 5).

set_coordinates(Coordinates, PlayerComponentData) ->
  NewPlayerComponentData = pewpew_player_component_data:update(Coordinates, PlayerComponentData),
  {ok, NewPlayerComponentData}.

get_coordinates(PlayerComponentData) ->
  X = pewpew_player_component_data:x(PlayerComponentData),
  Y = pewpew_player_component_data:y(PlayerComponentData),

  {ok, {x, X, y, Y}}.

move(<<"forward">>, PlayerComponentData) ->
  move_player(1, PlayerComponentData);
move(<<"backward">>, PlayerComponentData) ->
  move_player(-1, PlayerComponentData).

snapshot(PlayerComponentData) ->
  Snapshot = pewpew_player_component_snapshot:new(PlayerComponentData),

  {ok, Snapshot}.

update(PlayerComponentData, UpdateContext) ->
  #{shots := Shots} = UpdateContext,

  UpdatedData = evaluate_shots(Shots, PlayerComponentData),
  {ok, UpdatedData}.

configure(PlayerComponentData, <<"radarType">>, [NewType]) ->
  %TODO: remove this check as this is also checked in the radar_mod file
  IsValidRadarType           = lists:member(NewType, ?RADAR_MODES),

  case IsValidRadarType of
    true ->
      RadarConfigData              = pewpew_player_component_data:radar_config_data(PlayerComponentData),
      {ok, UpdatedRadarConfigData} = pewpew_radar_component_mod:change_radar_mode(NewType, RadarConfigData),
      UpdatedPlayerComponentData   = pewpew_player_component_data:update(PlayerComponentData, [{radar_config_data, UpdatedRadarConfigData}]),
      {ok, UpdatedPlayerComponentData};
    false ->
      {error, PlayerComponentData}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
move_player(Sign, PlayerComponentData) ->
  NewCoordinates         = calculate_new_coordinates(Sign, ?MOVEMENT_SPEED, PlayerComponentData),
  NewPlayerComponentData = pewpew_player_component_data:update(PlayerComponentData, NewCoordinates),
  {ok, NewPlayerComponentData}.

calculate_new_coordinates(Sign, Speed, PlayerComponentData) ->
  Rotation = pewpew_player_component_data:rotation(PlayerComponentData),
  RadianRotation = (Rotation * math:pi()) / 180,
  DX       = Speed * math:cos(RadianRotation),
  DY       = Speed * math:sin(RadianRotation),
  X        = pewpew_player_component_data:x(PlayerComponentData),
  Y        = pewpew_player_component_data:y(PlayerComponentData),

  RoundedX = round_value(X + (DX * Sign), 5),
  RoundedY = round_value(Y + (DY * Sign), 5),

  [{x, RoundedX}, {y, RoundedY}].

round_value(Value, Precision) ->
  P = math:pow(10, Precision),
  round(Value * P) / P.

evaluate_shots(Shots, PlayerComponentData) ->
  InitialLife = pewpew_player_component_data:life(PlayerComponentData),
  evaluate_shots(Shots, InitialLife, PlayerComponentData, []).

evaluate_shots([], _, PlayerComponentData, _) ->
  PlayerComponentData;
evaluate_shots([Shot | Tail], Life, PlayerComponentData, Notifications) ->
  Radius = pewpew_player_component_data:radius(PlayerComponentData),
  PX = pewpew_player_component_data:x(PlayerComponentData),
  PY = pewpew_player_component_data:y(PlayerComponentData),

  PlayerCircle = {x, PX, y, PY, radius, Radius},
  Intersect = pewpew_utils:circles_intersect(Shot, PlayerCircle),

  case Intersect of
    true ->
      CurrentLife = pewpew_player_component_data:life(PlayerComponentData),
      NewLife = CurrentLife - ?LIFE_LOSS_BY_SHOT_HIT,
      evaluate_shots(Tail, NewLife, pewpew_player_component_data:update(PlayerComponentData, [{life, NewLife}]), Notifications);
    false ->
      evaluate_shots(Tail, Life, PlayerComponentData, Notifications)
  end.
