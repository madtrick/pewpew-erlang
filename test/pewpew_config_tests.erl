-module(pewpew_config_tests).
-include_lib("eunit/include/eunit.hrl").

-define(APPLICATION, pewpew).

can_be_initialized_with_proplist_test_() ->
  Config = [{key, [{nested_key, value}]}, {other, value}, {another, [1, 2]}],

  pewpew_config:init(Config),
  {ok, Env}   = application:get_env(?APPLICATION, config),

  ?_assertEqual(#{key => #{nested_key => value}, other => value, another => [1, 2]}, Env).

can_get_with_nested_keys_test_() ->
  Config = [{key, [{nested_key, value}]}],
  pewpew_config:init(Config),

  Value = pewpew_config:get([key, nested_key]),

  ?_assertEqual(value, Value).

can_return_default_value_on_missing_key_test_() ->
  pewpew_config:init([]),

  Value = pewpew_config:get(key, my_nice_default_value),

  ?_assertEqual(my_nice_default_value, Value).

can_set_a_value_test_() ->
  pewpew_config:init([]),

  pewpew_config:set(key, value),
  Value = pewpew_config:get(key),

  ?_assertEqual(value, Value).
