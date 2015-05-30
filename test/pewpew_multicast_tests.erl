-module(pewpew_multicast_tests).
-include_lib("eunit/include/eunit.hrl").

publishes_messages_in_given_channels_test_() ->
  {setup,
    fun () ->
        meck:new(pewpew_channel),
        meck:expect(pewpew_channel, send, 2, ok),
        pewpew_multicast:start_link()
    end,
    fun (_) ->
        meck:unload(pewpew_channel),
        pewpew_multicast:stop()
    end,
    fun (_) ->
        pewpew_multicast:publish(data, [ch1, ch2]),
        timer:sleep(100),
        [
          ?_assertEqual(meck:called(pewpew_channel, send, [ch1, data]), true),
          ?_assertEqual(meck:called(pewpew_channel, send, [ch2, data]), true)
        ]
    end}.
