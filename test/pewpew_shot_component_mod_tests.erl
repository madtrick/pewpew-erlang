-module(pewpew_shot_component_mod_tests).
-include_lib("eunit/include/eunit.hrl").

shot_does_not_hit_wall_test_() ->
  ShotComponentData = pewpew_shot_component_data:new([{rotation, 0}, {x, 10}, {y, 10}]),
  UpdateContext = #{players => [], arena_dimensions => #{width => 20, height => 20}},
  {Status, _} = pewpew_shot_component_mod:update(ShotComponentData, UpdateContext),

  ?_assertEqual(ok, Status).

shot_does_not_hit_wall_2_test_() ->
  ShotComponentData = pewpew_shot_component_data:new([{rotation, 230}, {x, 10}, {y, 10}]),
  UpdateContext = #{players => [], arena_dimensions => #{width => 20, height => 20}},
  {Status, _} = pewpew_shot_component_mod:update(ShotComponentData, UpdateContext),

  ?_assertEqual(ok, Status).

shot_hits_wall_test_() ->
  ShotComponentData = pewpew_shot_component_data:new([{rotation, 0}, {x, 10}, {y, 10}]),
  UpdateContext = #{players => [], arena_dimensions => #{width => 10, height => 10}},
  {Status, _} = pewpew_shot_component_mod:update(ShotComponentData, UpdateContext),

  ?_assertEqual(destroy, Status).

shot_with_rotation_hits_wall_test_() ->
  ShotComponentData = pewpew_shot_component_data:new([{rotation, 180}, {x, 0}, {y, 0}]),
  UpdateContext = #{players => [], arena_dimensions => #{width => 10, height => 10}},
  {Status, _} = pewpew_shot_component_mod:update(ShotComponentData, UpdateContext),

  ?_assertEqual(destroy, Status).

shot_with_rotation_hits_wall_2_test_() ->
  ShotComponentData = pewpew_shot_component_data:new([{rotation, 180}, {x, 5}, {y, 10}]),
  UpdateContext = #{players => [], arena_dimensions => #{width => 10, height => 10}},
  {Status, _} = pewpew_shot_component_mod:update(ShotComponentData, UpdateContext),

  ?_assertEqual(destroy, Status).

shot_with_rotation_hits_wall_3_test_() ->
  ShotComponentData = pewpew_shot_component_data:new([{rotation, 180}, {x, 5}, {y, 0}]),
  UpdateContext = #{players => [], arena_dimensions => #{width => 10, height => 10}},
  {Status, _} = pewpew_shot_component_mod:update(ShotComponentData, UpdateContext),

  ?_assertEqual(destroy, Status).
