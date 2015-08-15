-module(pewpew_utils_tests).
-include_lib("eunit/include/eunit.hrl").

proplist_to_map_test_() ->
  Proplist = [{key, value}],
  Map      = #{key => value},

  ?_assertEqual(Map, pewpew_utils:proplist_to_map(Proplist)).

nested_proplist_to_map_test_() ->
  NestedProplist = [{key, [{nested_key, value}]}],
  NestedMap = #{key => #{nested_key => value}},

  ?_assertEqual(NestedMap, pewpew_utils:proplist_to_map(NestedProplist)).

get_nested_value_in_map_test_() ->
  Map = #{key => #{nested_key => value}},

  Value = pewpew_utils:get_value_in_map([key, nested_key], Map),

  ?_assertEqual(value, Value).
