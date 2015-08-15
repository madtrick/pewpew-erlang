-module(pewpew_config_tests).
-include_lib("eunit/include/eunit.hrl").

can_be_initialized_with_proplist_test_() ->
  Config = [{key, value}],

  pewpew_config:init(Config),
  Value = pewpew_config:get(key),

  ?_assertEqual(value, Value).

can_get_with_nested_keys_test_() ->
  Config = [{key, [{nested_key, value}]}],
  pewpew_config:init(Config),

  Value = pewpew_config:get([key, nested_key]),

  ?_assertEqual(value, Value).

can_set_a_value_test_() ->
  pewpew_config:init([]),

  pewpew_config:set(key, value),
  Value = pewpew_config:get(key),

  ?_assertEqual(value, Value).
