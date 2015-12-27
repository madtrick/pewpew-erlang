-module(pewpew_player_component_mod).
-include_lib("eunit/include/eunit.hrl").

-export([
  bounding_circle/1,
  set_coordinates/2,
  get_coordinates/1,
  move/2,
  snapshot/1,
  update/2,
  configure/3,
  shoot/1,
  hit/2
]).

-define(MOVEMENT_SPEED, 1).
% TODO remove the RADAR_MODES constant
-define(RADAR_MODES, [<<"long_range_scan">>]).
-define(LIFE_LOSS_BY_SHOT_HIT, 5).

bounding_circle(PlayerComponentData) ->
  X = pewpew_player_component_data:x(PlayerComponentData),
  Y = pewpew_player_component_data:y(PlayerComponentData),
  Radius = pewpew_player_component_data:radius(PlayerComponentData),
  BoundingCircle = {x, X, y, Y, radius, Radius},

  {ok, BoundingCircle}.

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

update(PlayerComponentData, _UpdateContext) ->
  %#{shots := Shots} = UpdateContext,
  %Life = pewpew_player_component_data:life(PlayerComponentData),

  {Status, UPCD, Notifications} = evaluate_hits(PlayerComponentData),
  UPCD2 = pewpew_player_component_data:update(UPCD, [{hits, []}]),
  case Status of
    alive ->
      UpdatedPlayerComponentData = update_shooting_info(UPCD2),
      {ok, UpdatedPlayerComponentData, Notifications};
    destroyed ->
      {destroyed, UPCD2, Notifications}
  end.


  %evaluate_shots(Shots, UpdatedPlayerComponentData).

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

hit(PlayerComponentData, Hit) ->
  Hits = pewpew_player_component_data:hits(PlayerComponentData),
  NewHits = [Hit | Hits],
  UpdatedPlayerComponentData = pewpew_player_component_data:update(PlayerComponentData, [{hits, NewHits}]),

  {ok, UpdatedPlayerComponentData}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
move_player(Sign, PlayerComponentData) ->
  Speed                  = pewpew_player_component_data:speed(PlayerComponentData),
  NewCoordinates         = calculate_new_coordinates(Sign, Speed, PlayerComponentData),
  NewPlayerComponentData = pewpew_player_component_data:update(PlayerComponentData, NewCoordinates),
  {ok, NewPlayerComponentData}.

calculate_new_coordinates(Sign, Speed, PlayerComponentData) ->
  Rotation = pewpew_player_component_data:rotation(PlayerComponentData),
  Speed    = pewpew_player_component_data:speed(PlayerComponentData),
  Coordinates = {
      x, pewpew_player_component_data:x(PlayerComponentData),
      y, pewpew_player_component_data:y(PlayerComponentData)
      },

  {x, UpdatedX, y, UpdatedY} = pewpew_utils:translate_point_by_vector(Speed * Sign, Rotation, Coordinates),
  [{x, UpdatedX}, {y, UpdatedY}].

evaluate_hits(PlayerComponentData) ->
  Hits = pewpew_player_component_data:hits(PlayerComponentData),
  evaluate_hit(PlayerComponentData, Hits, []).

evaluate_hit(PlayerComponentData, [], Notifications) ->
  Life = pewpew_player_component_data:life(PlayerComponentData),
  case Life =< 0 of
    true ->
      PlayerDestroyedNotification = player_destroyed_notification(PlayerComponentData),
      {destroyed, PlayerComponentData, [PlayerDestroyedNotification | Notifications]};
    false ->
      {alive, PlayerComponentData, Notifications}
  end;
evaluate_hit(PlayerComponentData, [_ | Hits], Notifications) ->
  Life = pewpew_player_component_data:life(PlayerComponentData),
  NewLife = Life - ?LIFE_LOSS_BY_SHOT_HIT,
  NewPlayerComponentData = pewpew_player_component_data:update(PlayerComponentData, [{life, NewLife}]),
  Notification = player_hit_by_shot_notification(NewPlayerComponentData),
  evaluate_hit(NewPlayerComponentData, Hits, [Notification | Notifications]).

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
  #{
    tokens := Tokens,
    max_tokens := MaxTokens,
    new_tokens_per_cycle := NewTokensPerCycle
    } = ShootingInfo,

  update_shooting_info(PlayerComponentData, MaxTokens, Tokens, NewTokensPerCycle).

update_shooting_info(PlayerComponentData, MaxTokens, CurrentTokens, NewTokensPerCycle) when (CurrentTokens + NewTokensPerCycle) >= MaxTokens->
  PlayerComponentData;
update_shooting_info(PlayerComponentData, _MaxTokens, CurrentTokens, NewTokensPerCycle) ->
  ShootingInfo = pewpew_player_component_data:shooting_info(PlayerComponentData),
  UpdatedTokensValue = CurrentTokens + NewTokensPerCycle,
  UpdatedShootingInfo = maps:put(tokens, UpdatedTokensValue, ShootingInfo),

  pewpew_player_component_data:update(PlayerComponentData, [{shooting_info, UpdatedShootingInfo}]).
