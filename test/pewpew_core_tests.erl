-module(pewpew_core_tests).
-include_lib("eunit/include/eunit.hrl").

it_only_stores_one_message_per_player_test_() ->
  {setup,
    fun() ->
        meck:new(pewpew_games_sup),
        meck:expect(pewpew_games_sup, add_game, 1, ok),
        pewpew_core:start_link()
    end,
    fun(_) ->
        meck:unload(pewpew_games_sup),
        pewpew_core:stop()
    end,
    fun(_) ->
        pewpew_core:process_player_message({text, msg1}, ch1),
        pewpew_core:process_player_message({text, msg2}, ch1),
        pewpew_core:process_player_message({text, msg1}, ch2),

        [
         ?_assertEqual(2, pewpew_core:number_of_pending_messages()),
         ?_assertEqual(1, pewpew_core:number_of_pending_messages_per_channel(ch1)),
         ?_assertEqual(1, pewpew_core:number_of_pending_messages_per_channel(ch2))
        ]
    end
  }.

it_clears_the_players_message_queue_after_each_game_cycle_test_() ->
  MockedModules = [
    pewpew_games_sup,
    pewpew_command_parser,
    pewpew_command_context_data,
    pewpew_command_runner,
    pewpew_game,
    pewpew_message_dispatcher
  ],

  {setup,
    fun() ->
        meck:new(MockedModules),
        meck:expect(pewpew_games_sup, add_game, 1, ok),
        meck:expect(pewpew_command_parser, parse, 1, commmand_context),
        meck:expect(pewpew_command_context_data, update, 2, updated_commmand_context),
        meck:expect(pewpew_command_runner, run, 1, noreply),
        meck:expect(pewpew_game, snapshot, 1, snapshot),
        meck:expect(pewpew_message_dispatcher, dispatch, 1, ok),
        pewpew_core:start_link()
    end,
    fun(_) ->
        meck:unload(MockedModules),
        pewpew_core:stop()
    end,
    fun(_) ->
        pewpew_core:process_player_message({text, msg1}, ch1),
        pewpew_core:process_player_message({text, msg2}, ch1),
        pewpew_core:process_player_message({text, msg1}, ch2),

        pewpew_core:next_cycle(),

        [
         ?_assertEqual(0, pewpew_core:number_of_pending_messages()),
         ?_assertEqual(0, pewpew_core:number_of_pending_messages_per_channel(ch1)),
         ?_assertEqual(0, pewpew_core:number_of_pending_messages_per_channel(ch2))
        ]
    end
  }.
