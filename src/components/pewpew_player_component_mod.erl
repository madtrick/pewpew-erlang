-module(pewpew_player_component_mod).

-export([
  set_coordinates/2,
  get_coordinates/1,
  move/2,
  snapshot/1,
  update/1
]).

-define(MOVEMENT_SPEED, 1).

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

update(_PlayerComponentData) ->
  {ok, noupdate}.

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

  [{x, X + (DX * Sign)}, {y, Y + (DY * Sign)}].
