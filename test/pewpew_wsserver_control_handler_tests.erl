-module(pewpew_wsserver_control_handler_tests).
-include_lib("eunit/include/eunit.hrl").

registers_the_handler_test_() ->
  {setup,
    fun() ->
        meck:new([pewpew_core]),
        meck:expect(pewpew_core, register_control_channel, 1, ok)
    end,
    fun(_) ->
        meck:unload([pewpew_core])
    end,
    fun(_) ->
        pewpew_wsserver_control_handler:init([{worker, wk1}]),
        ?_assert(meck:validate(pewpew_core))
    end
  }.
