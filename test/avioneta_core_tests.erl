-module(avioneta_core_tests).
-include_lib("eunit/include/eunit.hrl").

publishes_messages_to_players_test_() ->
  {setup,
    fun () ->
        meck:new([avioneta_multicast, avioneta_registry, avioneta_command_parser, avioneta_command_runner, avioneta_order_dispatcher]),
        meck:expect(avioneta_command_parser, parse, 1, command),
        meck:expect(avioneta_multicast, publish, 2, ok),
        meck:expect(avioneta_registry, entries, 0, [ch1, ch2, ch3]),
        meck:expect(avioneta_command_runner, run, 2, {new_game_state, orders}),
        meck:expect(avioneta_order_dispatcher, dispatch, 3, ok),
        avioneta_core:start_link()
    end,
    fun (_) ->
        meck:unload([avioneta_multicast, avioneta_registry, avioneta_command_parser, avioneta_command_runner, avioneta_order_dispatcher])
    end,
    fun (_) ->
        avioneta_core:process_message({text, message}, ch3),
        timer:sleep(100),
        [
          ?_assertEqual(meck:called(avioneta_multicast, publish, [message, [ch1, ch2]]), true)
        ]
    end}.
