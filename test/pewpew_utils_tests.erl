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
