-module(pewpew_wsserver_player_handler_tests).
-include_lib("eunit/include/eunit.hrl").

it_creates_a_channel_when_initialized_test_() ->
  {setup,
    fun() ->
        meck:new([pewpew_channel]),
        meck:expect(pewpew_channel, create, 2, {ok, ch1})
    end,
    fun(_) ->
        meck:unload([pewpew_channel])
    end,
    fun(_) ->
        pewpew_wsserver_player_handler:init([{worker, wk1}]),
        [
          ?_assert(meck:called(pewpew_channel, create, [wk1, [{is_control, false}]]))
        ]
    end}.

passes_messages_to_core_for_processing_test_() ->
  {setup,
    fun() ->
        meck:new([pewpew_channel, pewpew_core]),
        meck:expect(pewpew_channel, create, 2, {ok, ch1}),
        meck:expect(pewpew_core, process_player_message, 2, ok),

        pewpew_wsserver_player_handler:init([{worker, wk1}])
    end,
    fun(_) ->
        meck:unload([pewpew_channel, pewpew_core])
    end,
    fun(State) ->
        Message = {text, message},
        pewpew_wsserver_player_handler:handle(Message, State),
        [
          ?_assert(meck:called(pewpew_core, process_player_message, [Message, ch1]))
        ]
    end}.

it_terminates_the_channel_when_the_connection_is_closed_test_() ->
  {setup,
    fun() ->
        meck:new([pewpew_channel, pewpew_core]),
        meck:expect(pewpew_channel, create, 2, {ok, ch1}),
        meck:expect(pewpew_channel, exit, 1, ok),
        meck:expect(pewpew_core, process_player_message, 2, ok),

        pewpew_wsserver_player_handler:init([{worker, wk1}])
    end,
    fun(_) ->
        meck:unload([pewpew_channel, pewpew_core])
    end,
    fun(State) ->
        pewpew_wsserver_player_handler:handle(connection_close, State),
        [
          ?_assert(meck:called(pewpew_channel, exit, [ch1]))
        ]
    end}.

it_terminates_the_channel_when_the_connection_is_closed_2_test_() ->
  {setup,
    fun() ->
        meck:new([pewpew_channel, pewpew_core]),
        meck:expect(pewpew_channel, create, 2, {ok, ch1}),
        meck:expect(pewpew_channel, exit, 1, ok),
        meck:expect(pewpew_core, process_player_message, 2, ok),

        pewpew_wsserver_player_handler:init([{worker, wk1}])
    end,
    fun(_) ->
        meck:unload([pewpew_channel, pewpew_core])
    end,
    fun(State) ->
        pewpew_wsserver_player_handler:handle({close, reason}, State),
        [
          ?_assert(meck:called(pewpew_channel, exit, [ch1]))
        ]
    end}.
