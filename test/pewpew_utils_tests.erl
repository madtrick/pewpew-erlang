-module(pewpew_utils_tests).
-include_lib("eunit/include/eunit.hrl").

proplist_to_map_test_() ->
  Proplist = [{key, value}],
  Map      = #{key => value},

  ?_assertEqual(Map, pewpew_utils:proplist_to_map(Proplist)).

get_nested_value_in_map_test_() ->
  Map = #{key => #{nested_key => value}},

  Value = pewpew_utils:get_value_in_map([key, nested_key], Map),

  ?_assertEqual(value, Value).

return_default_value_on_undefined_key_in_map_get_test_() ->
  Map = #{},

  Value = pewpew_utils:get_value_in_map(lol, Map, undefined),

  ?_assertEqual(undefined, Value).

set_value_in_map_test_() ->
  Map = #{},

  NewMap = pewpew_utils:set_value_in_map(key, value, Map),
  Value = pewpew_utils:get_value_in_map(key, NewMap),

  ?_assertEqual(value, Value).

set_value_in_map_for_existing_nested_key_test_() ->
  Map = #{key => #{nested_key => something, other => value}},

  NewMap = pewpew_utils:set_value_in_map([key, nested_key], value, Map),

  ?_assertEqual(#{key => #{nested_key => value, other => value}}, NewMap).

set_value_in_map_for_non_existing_nested_key_test_() ->
  Map = #{},

  NewMap = pewpew_utils:set_value_in_map([key, nested_key], value, Map),

  ?_assertEqual(#{key => #{nested_key => value}}, NewMap).

circles_intersect_test_() ->
  Circle1 = {x, 200, y, 200, radius, 5},
  Circle2 = {x, 206.5, y, 200, radius, 1},

  Intersect = pewpew_utils:circles_intersect(Circle1, Circle2),

  ?_assert(not Intersect).

contact_points_between_line_and_circle_test() ->
  Line   = {x, 4, y, 0, rotation, 0},
  Circle = {x, 5, y, 0, radius, 2},

  Results = pewpew_utils:contact_points_between_line_and_circle(Line, Circle),

  ?assertEqual([{x, 3.0, y, 0.0}, {x, 7.0, y, 0.0}], Results).

contact_points_between_line_and_circle_2_test() ->
  Line   = {x, 0, y, 0, rotation, 90},
  Circle = {x, 0, y, 5, radius, 2},

  Results = pewpew_utils:contact_points_between_line_and_circle(Line, Circle),

  ?assertEqual([{x, 0.0, y, 3.0}, {x, 0.0, y, 7.0}], Results).

contact_points_between_line_and_circle_3_test() ->
  Line   = {x, 0, y, 0, rotation, 45},
  Circle = {x, 2, y, 2, radius, 1},

  Results = pewpew_utils:contact_points_between_line_and_circle(Line, Circle),

  ?assertEqual([{x, 1.29289, y, 1.29289}, {x, 2.70711, y, 2.70711}], Results).

contact_points_between_line_and_circle_4_test() ->
  Line   = {x, 0, y, 3, rotation, 0},
  Circle = {x, 2, y, 2, radius, 1},

  Results = pewpew_utils:contact_points_between_line_and_circle(Line, Circle),

  ?assertEqual([{x, 2.0, y, 3.0}], Results).

contact_points_between_line_and_circle_5_test() ->
  Line   = {x, 0, y, 30, rotation, 0},
  Circle = {x, 2, y, 2, radius, 1},

  Results = pewpew_utils:contact_points_between_line_and_circle(Line, Circle),

  ?assertEqual([], Results).

contact_points_between_segment_and_circle_test() ->
  Segment = [{x, 0, y, 0}, {x, 2, y, 2}],
  Circle = {x, 2, y, 2, radius, 1},

  Results = pewpew_utils:contact_points_between_segment_and_circle(Segment, Circle),

  ?assertEqual([{x, 1.29289, y, 1.29289}], Results).

contact_points_between_segment_and_circle_2_test() ->
  Segment = [{x, 0, y, 2}, {x, 4, y, 2}],
  Circle = {x, 2, y, 2, radius, 1},

  Results = pewpew_utils:contact_points_between_segment_and_circle(Segment, Circle),

  ?assertEqual([{x, 1.0, y, 2.0}, {x, 3.0, y, 2.0}], Results).

contact_points_between_segment_and_circle_3_test() ->
  Segment = [{x, 2, y, 0}, {x, 2, y, 4}],
  Circle = {x, 2, y, 2, radius, 1},

  Results = pewpew_utils:contact_points_between_segment_and_circle(Segment, Circle),

  ?assertEqual([{x, 2.0, y, 1.0}, {x, 2.0, y, 3.0}], Results).

contact_points_between_segment_and_circle_4_test() ->
  Segment = [{x, 2, y, 2}, {x, 4, y, 2}],
  Circle = {x, 2, y, 2, radius, 1},

  Results = pewpew_utils:contact_points_between_segment_and_circle(Segment, Circle),

  ?assertEqual([{x, 3.0, y, 2.0}], Results).

contact_points_between_segment_and_circle_5_test() ->
  Segment = [{x, 1, y, 1}, {x, 3, y, 3}],
  Circle = {x, 2, y, 2, radius, 1},

  Results = pewpew_utils:contact_points_between_segment_and_circle(Segment, Circle),

  ?assertEqual([{x,1.29289,y,1.29289},{x,2.70711,y,2.70711}], Results).

contact_points_between_segment_and_circle_6_test() ->
  Segment = [{x, 1, y, 1}, {x, 2, y, 2}],
  Circle = {x, 2, y, 2, radius, 1},

  Results = pewpew_utils:contact_points_between_segment_and_circle(Segment, Circle),

  ?assertEqual([{x,1.29289,y,1.29289}], Results).

contact_points_between_segment_and_circle_7_test() ->
  Segment = [{x, 1, y, 4}, {x, 1, y, 1}],
  Circle = {x, 2, y, 2, radius, 1},

  Results = pewpew_utils:contact_points_between_segment_and_circle(Segment, Circle),

  ?assertEqual([{x, 1.0, y, 2.0}], Results).

contact_points_between_segment_and_circle_8_test() ->
  Segment = [{x, 1, y, 2}, {x, 2, y, 2}],
  Circle = {x, 2, y, 2, radius, 1},

  Results = pewpew_utils:contact_points_between_segment_and_circle(Segment, Circle),

  ?assertEqual([{x, 1.0, y, 2.0}], Results).

contact_points_between_segment_and_circle_9_test() ->
  Segment = [{x, 2, y, 1}, {x, 2, y, 2}],
  Circle = {x, 2, y, 2, radius, 1},

  Results = pewpew_utils:contact_points_between_segment_and_circle(Segment, Circle),

  ?assertEqual([{x, 2.0, y, 1.0}], Results).
