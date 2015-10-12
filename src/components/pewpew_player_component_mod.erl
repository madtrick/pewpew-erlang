-module(pewpew_player_component_mod).
-include_lib("eunit/include/eunit.hrl").

-export([
  set_coordinates/2,
  get_coordinates/1,
  move/2,
  snapshot/1,
  update/2,
  configure/3,
  shoot/1
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

  UpdatedPlayerComponentData = update_shooting_info(PlayerComponentData),
  evaluate_shots(Shots, UpdatedPlayerComponentData).

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

shoot(PlayerComponentData) ->
  ShootingInfo = pewpew_player_component_data:shooting_info(PlayerComponentData),

  #{
    cost := ShootingCost,
    tokens := ShootingTokens
    } = ShootingInfo,

  UpdatedShootingTokens = ShootingTokens - ShootingCost,
  UpdatedShootingInfo = maps:put(tokens, UpdatedShootingTokens, ShootingInfo),
  UpdatedPlayerComponentData = pewpew_player_component_data:update(PlayerComponentData, [{shooting_info, UpdatedShootingInfo}]),

  {ok, UpdatedPlayerComponentData}.

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

  RoundedX = pewpew_utils:round_value(X + (DX * Sign), 5),
  RoundedY = pewpew_utils:round_value(Y + (DY * Sign), 5),

  [{x, RoundedX}, {y, RoundedY}].

evaluate_shots(Shots, PlayerComponentData) ->
  InitialLife = pewpew_player_component_data:life(PlayerComponentData),
  evaluate_shots(Shots, InitialLife, PlayerComponentData, []).

evaluate_shots([], _, PlayerComponentData, Notifications) ->
  {ok, PlayerComponentData, Notifications};
evaluate_shots([Shot | Tail], Life, PlayerComponentData, Notifications) ->
  Radius = pewpew_player_component_data:radius(PlayerComponentData),
  PX = pewpew_player_component_data:x(PlayerComponentData),
  PY = pewpew_player_component_data:y(PlayerComponentData),
  {x, ShotX, y, ShotY, radius, ShotRadius} = Shot,

  PlayerCircle = {x, PX, y, PY, radius, Radius},
  ShotCircle = {x, ShotX, y, ShotY, radius, ShotRadius},
  Intersect = pewpew_utils:circles_intersect(ShotCircle, PlayerCircle),

  case Intersect of
    true ->
      CurrentLife = pewpew_player_component_data:life(PlayerComponentData),
      NewLife = CurrentLife - ?LIFE_LOSS_BY_SHOT_HIT,
      UpdatedPlayerComponentData = pewpew_player_component_data:update(PlayerComponentData, [{life, NewLife}]),
      case NewLife =< 0 of
        false ->
          Notification = player_hit_by_shot_notification(UpdatedPlayerComponentData),
          UpdatedNotifications = [Notification | Notifications],
          evaluate_shots(Tail, NewLife, UpdatedPlayerComponentData, UpdatedNotifications);
        true ->
          Notification = player_destroyed_notification(UpdatedPlayerComponentData),
          UpdatedNotifications = [Notification | Notifications],
          {destroyed, UpdatedPlayerComponentData, UpdatedNotifications}
      end;
    false ->
      evaluate_shots(Tail, Life, PlayerComponentData, Notifications)
  end.

player_hit_by_shot_notification(PlayerComponentData) ->
  Life = pewpew_player_component_data:life(PlayerComponentData),
  PlayerData = {life, Life},
  Channel = pewpew_player_component_data:origin(PlayerComponentData),

  {player, Channel, update, {notification, pewpew_player_hit_by_shot_notification:new(PlayerData)}}.

player_destroyed_notification(PlayerComponentData) ->
  Channel = pewpew_player_component_data:origin(PlayerComponentData),
  {player, Channel, destroyed, {notification, pewpew_player_destroyed_notification:new()}}.

update_shooting_info(PlayerComponentData) ->
  ShootingInfo = pewpew_player_component_data:shooting_info(PlayerComponentData),
  #{ tokens := Tokens, new_tokens_per_cycle := NewTokensPerCycle } = ShootingInfo,

  UpdatedTokensValue = Tokens + NewTokensPerCycle,
  UpdatedShootingInfo = maps:put(tokens, UpdatedTokensValue, ShootingInfo),

  pewpew_player_component_data:update(PlayerComponentData, [{shooting_info, UpdatedShootingInfo}]).
