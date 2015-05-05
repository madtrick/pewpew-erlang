-module(pewpew_core_tests).
-include_lib("eunit/include/eunit.hrl").

publishes_messages_to_players_test_() ->
  {setup,
    fun () ->
        meck:new([pewpew_multicast, pewpew_registry, pewpew_command_parser, pewpew_command_runner, pewpew_order_dispatcher]),
        meck:expect(pewpew_command_parser, parse, 1, command),
        meck:expect(pewpew_multicast, publish, 2, ok),
        meck:expect(pewpew_registry, entries, 0, [ch1, ch2, ch3]),
        meck:expect(pewpew_command_runner, run, 2, {new_game_state, orders}),
        meck:expect(pewpew_order_dispatcher, dispatch, 3, ok),
        pewpew_core:start_link()
    end,
    fun (_) ->
        meck:unload([pewpew_multicast, pewpew_registry, pewpew_command_parser, pewpew_command_runner, pewpew_order_dispatcher])
    end,
    fun (_) ->
        pewpew_core:process_message({text, message}, ch3),
        timer:sleep(100),
        [
          ?_assertEqual(meck:called(pewpew_multicast, publish, [message, [ch1, ch2]]), true)
        ]
    end}.
