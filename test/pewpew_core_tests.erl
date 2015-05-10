-module(pewpew_core_tests).
-include_lib("eunit/include/eunit.hrl").

it_only_stores_one_message_per_sender_test_() ->
  {setup,
    fun() ->
        meck:new(pewpew_games_sup),
        meck:expect(pewpew_games_sup, add_game, 1, ok),
        pewpew_core:start_link()
    end,
    fun(_) ->
        meck:unload(pewpew_games_sup)
    end,
    fun(_) ->
        pewpew_core:process_message({text, msg1}, ch1),
        pewpew_core:process_message({text, msg2}, ch1),
        pewpew_core:process_message({text, msg1}, ch2),

        [
         ?_assertEqual(2, pewpew_core:number_of_pending_messages()),
         ?_assertEqual(1, pewpew_core:number_of_pending_messages_per_channel(ch1)),
         ?_assertEqual(1, pewpew_core:number_of_pending_messages_per_channel(ch2))
        ]
    end
  }.

it_clears_the_message_queue_after_each_game_cycle_test_() ->
  {setup,
    fun() ->
        meck:new(pewpew_games_sup),
        meck:expect(pewpew_games_sup, add_game, 1, ok),
        pewpew_core:start_link()
    end,
    fun(_) ->
        meck:unload(pewpew_games_sup)
    end,
    fun(_) ->
        pewpew_core:process_message({text, msg1}, ch1),
        pewpew_core:process_message({text, msg2}, ch1),
        pewpew_core:process_message({text, msg1}, ch2),

        pewpew_core:next_cycle(),

        [
         ?_assertEqual(0, pewpew_core:number_of_pending_messages()),
         ?_assertEqual(0, pewpew_core:number_of_pending_messages_per_channel(ch1)),
         ?_assertEqual(0, pewpew_core:number_of_pending_messages_per_channel(ch2))
        ]
    end
  }.
