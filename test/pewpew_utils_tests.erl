-module(pewpew_utils_tests).
-include_lib("eunit/include/eunit.hrl").

proplist_to_map_test_() ->
  Proplist = [{key, value}],
  Map      = #{key => value},

  ?_assertEqual(Map, pewpew_utils:proplist_to_map(Proplist)).
