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
