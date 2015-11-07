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

it_terminates_the_channel_when_the_connection_is_closed_test_() ->
  {setup,
    fun() ->
        meck:expect(pewpew_channel, create, 2, {ok, ch1}),
        meck:expect(pewpew_channel, exit, 1, ok),

        pewpew_wsserver_control_handler:init([{worker, wk1}])
    end,
    fun(_) ->
        meck:unload([pewpew_channel])
    end,
    fun(State) ->
        pewpew_wsserver_control_handler:handle(connection_close, State),
        [
          ?_assert(meck:called(pewpew_channel, exit, [ch1]))
        ]
    end}.

it_terminates_the_channel_when_the_connection_is_closed_2_test_() ->
  {setup,
    fun() ->
        meck:new([pewpew_channel]),
        meck:expect(pewpew_channel, create, 2, {ok, ch1}),
        meck:expect(pewpew_channel, exit, 1, ok),

        pewpew_wsserver_control_handler:init([{worker, wk1}])
    end,
    fun(_) ->
        meck:unload([pewpew_channel])
    end,
    fun(State) ->
        pewpew_wsserver_control_handler:handle({close, reason}, State),
        [
          ?_assert(meck:called(pewpew_channel, exit, [ch1]))
        ]
    end}.
