-module(pewpew_handler_tests).
-include_lib("eunit/include/eunit.hrl").

it_creates_a_channel_when_initialized_test_() ->
  {setup,
    fun() ->
        meck:new([pewpew_channel, pewpew_registry]),
        meck:expect(pewpew_channel, create, 1, {ok, ch1})
    end,
    fun(_) ->
        meck:unload([pewpew_channel, pewpew_registry])
    end,
    fun(_) ->
        pewpew_handler:init([{worker, wk1}]),
        [
          ?_assertEqual(meck:called(pewpew_channel, create, [wk1]), true)
        ]
    end}.

passes_messages_to_core_for_processing_test_() ->
  {setup,
    fun() ->
        meck:new([pewpew_channel, pewpew_core]),
        meck:expect(pewpew_channel, create, 1, {ok, ch1}),
        meck:expect(pewpew_core, process_message, 2, ok),

        pewpew_handler:init([{worker, wk1}])
    end,
    fun(_) ->
        meck:unload([pewpew_channel, pewpew_core])
    end,
    fun(State) ->
        Message = {text, message},
        pewpew_handler:handle(Message, State),
        [
          ?_assertEqual(meck:called(pewpew_core, process_message, [Message, ch1]), true)
        ]
    end}.
