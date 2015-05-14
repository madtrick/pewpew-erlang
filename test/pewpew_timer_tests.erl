-module(pewpew_timer_tests).
-include_lib("eunit/include/eunit.hrl").

executes_the_callback_function_after_the_tick_period_test_() ->
  {setup,
    fun() ->
      pewpew_timer:start_link(),

      meck:new(pewpew_timer_callback_mock),
      meck:expect(pewpew_timer_callback_mock, callback, 0, ok)
    end,
    fun(_) ->
      meck:unload(pewpew_timer_callback_mock)
    end,
    fun(_) ->
      pewpew_timer:tick_every(pewpew_timer_callback_mock, callback),

      timer:sleep(60),

      [
        ?_assertEqual(3, meck:num_calls(pewpew_timer_callback_mock, callback, 0))
      ]
    end}.

