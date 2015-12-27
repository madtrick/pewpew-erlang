-module(pewpew_shot_component_mod).
-include_lib("eunit/include/eunit.hrl").

-export([
  get_coordinates/1,
  update/2,
  move/1,
  snapshot/1
]).

get_coordinates(ShotComponentData) ->
  X = pewpew_shot_component_data:x(ShotComponentData),
  Y = pewpew_shot_component_data:y(ShotComponentData),

  {ok, {x, X, y, Y}}.

move(ShotComponentData) ->
  Rotation    = pewpew_shot_component_data:rotation(ShotComponentData),
  X           = pewpew_shot_component_data:x(ShotComponentData),
  Y           = pewpew_shot_component_data:y(ShotComponentData),
  Speed       = pewpew_shot_component_data:speed(ShotComponentData),
  Coordinates = {x, X, y, Y},

  {x, UpdatedX, y, UpdatedY} = pewpew_utils:translate_point_by_vector(Speed, Rotation, Coordinates),

  UpdatedShotComponentData = pewpew_shot_component_data:update(ShotComponentData, [
        {x, UpdatedX},
        {y, UpdatedY}
        ]),
  {ok, UpdatedShotComponentData}.

update(ShotComponentData, UpdateContext) ->
  #{ arena_dimensions := ArenaDimensions, players := Players} = UpdateContext,
  X = pewpew_shot_component_data:x(ShotComponentData),
  Y = pewpew_shot_component_data:y(ShotComponentData),

  case is_out_of_bounds(X, Y, ArenaDimensions) of
    true ->
      {destroy, ShotComponentData};
    false ->
      %ShotCircleDescriptor = {x, X, y, Y, radius, 1},
      %HitsPlayer = lists:any(fun(Player) ->
      %        PlayerBoundingCircle = pewpew_player_component:bounding_circle(Player),
      %  pewpew_utils:circles_intersect(PlayerBoundingCircle, ShotCircleDescriptor)
      % end, Players),
      case check_if_collides_with_any_player(ShotComponentData, Players) of
        {true, Player} ->
          hit_player(ShotComponentData, Player),
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
check_if_collides_with_any_player(_, []) ->
  false;
check_if_collides_with_any_player(ShotComponentData, [Player | Players]) ->
  X = pewpew_shot_component_data:x(ShotComponentData),
  Y = pewpew_shot_component_data:y(ShotComponentData),
  PlayerBoundingCircle = pewpew_player_component:bounding_circle(Player),
  ShotCircleDescriptor = {x, X, y, Y, radius, 1},
  Collision = pewpew_utils:circles_intersect(PlayerBoundingCircle, ShotCircleDescriptor),

  case Collision of
    true -> {true, Player};
    false -> check_if_collides_with_any_player(ShotComponentData, Players)
  end.

hit_player(_ShotComponentData, Player) ->
  %% the atom 'hit' does nothing right now. In the future we could replace it
  %% with the coordinates of the host
  pewpew_player_component:hit(Player, hit).

is_out_of_bounds(X, Y, ArenaDimensions) ->
  #{ width := ArenaWidth, height := ArenaHeight } = ArenaDimensions,

  Y =< 0 orelse
  X =< 0 orelse
  X >= ArenaWidth orelse
  Y >= ArenaHeight.
