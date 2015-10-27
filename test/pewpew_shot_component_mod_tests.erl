-module(pewpew_shot_component_mod_tests).
-include_lib("eunit/include/eunit.hrl").

movements_take_speed_into_account_test_() ->
  Speeds = [1, 2],

  lists:map(fun(Speed) ->
        ShotComponentData = pewpew_shot_component_data:new([{speed, Speed}, {rotation, 0}, {x, 0}, {y, 0}]),
        {ok, UpdatedShotComponentData} = pewpew_shot_component_mod:move(ShotComponentData),
        UpdatedX = pewpew_shot_component_data:x(UpdatedShotComponentData),
        UpdatedY = pewpew_shot_component_data:y(UpdatedShotComponentData),

        [
          ?_assertEqual(Speed * 1.0, UpdatedX),
          ?_assertEqual(0.0, UpdatedY)
          ]
    end, Speeds).

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
