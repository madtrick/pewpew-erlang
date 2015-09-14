-module(pewpew_shot_component_mod_tests).
-include_lib("eunit/include/eunit.hrl").

shot_does_not_hit_wall_test_() ->
  ShotComponentData = pewpew_shot_component_data:new([{rotation, 0}, {x, 10}, {y, 10}]),
  UpdateContext = #{arena_dimensions => #{width => 20, height => 20}},
  {Status, _} = pewpew_shot_component_mod:update(ShotComponentData, UpdateContext),

  ?_assertEqual(ok, Status).

shot_does_not_hit_wall_2_test_() ->
  ShotComponentData = pewpew_shot_component_data:new([{rotation, 230}, {x, 10}, {y, 10}]),
  UpdateContext = #{arena_dimensions => #{width => 20, height => 20}},
  {Status, _} = pewpew_shot_component_mod:update(ShotComponentData, UpdateContext),

  ?_assertEqual(ok, Status).

shot_hits_wall_test_() ->
  ShotComponentData = pewpew_shot_component_data:new([{rotation, 0}, {x, 10}, {y, 10}]),
  UpdateContext = #{arena_dimensions => #{width => 10, height => 10}},
  {Status, _} = pewpew_shot_component_mod:update(ShotComponentData, UpdateContext),

  ?_assertEqual(destroy, Status).

shot_with_rotation_hits_wall_test_() ->
  ShotComponentData = pewpew_shot_component_data:new([{rotation, 180}, {x, 1}, {y, 0}]),
  UpdateContext = #{arena_dimensions => #{width => 10, height => 10}},
  {Status, _} = pewpew_shot_component_mod:update(ShotComponentData, UpdateContext),

  ?_assertEqual(destroy, Status).

shot_hits_wall_2_test_() ->
  ShotComponentData = pewpew_shot_component_data:new([{rotation, 0}, {x, 0}, {y, 0}]),
  UpdateContext = #{arena_dimensions => #{width => 10, height => 10}},

  Speed = 1.5,
  Steps = pewpew_utils:ceil(10 / Speed),
  Range = lists:seq(1, Steps),

  {Tests, _} = lists:foldl(fun (N, {Objects, Data}) ->
    {Status, UpdatedData} = pewpew_shot_component_mod:update(Data, UpdateContext),

    case N =:= Steps of
      true ->
        {[?_assertEqual(destroy, Status) | Objects], UpdatedData};
      false ->
        {[?_assertEqual(ok, Status) | Objects], UpdatedData}
    end
  end, {[], ShotComponentData}, Range),

  Tests.
