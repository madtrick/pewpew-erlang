-module(avioneta_multicast_tests).
-include_lib("eunit/include/eunit.hrl").

publishes_messages_in_given_channels_test_() ->
  {setup,
    fun () ->
        meck:new(avioneta_channel),
        meck:expect(avioneta_channel, send, 2, ok),
        avioneta_multicast:start_link()
    end,
    fun (_) ->
        meck:unload(avioneta_channel)
    end,
    fun (_) ->
        avioneta_multicast:publish(data, [ch1, ch2]),
        timer:sleep(100),
        [
          ?_assertEqual(meck:called(avioneta_channel, send, [ch1, data]), true),
          ?_assertEqual(meck:called(avioneta_channel, send, [ch2, data]), true)
        ]
    end}.
