-module(avioneta_handler_tests).
-include_lib("eunit/include/eunit.hrl").

it_creates_a_channel_when_initialized_test_() ->
  {setup,
    fun() ->
        meck:new([avioneta_channel, avioneta_registry]),
        meck:expect(avioneta_channel, create, 1, {ok, ch1})
    end,
    fun(_) ->
        meck:unload([avioneta_channel])
    end,
    fun(_) ->
        avioneta_handler:init([{worker, wk1}]),
        [
          ?_assertEqual(meck:called(avioneta_channel, create, [wk1]), true)
        ]
    end}.

passes_messages_to_core_for_processing_test_() ->
  {setup,
    fun() ->
        meck:new([avioneta_channel, avioneta_core]),
        meck:expect(avioneta_channel, create, 1, {ok, ch1}),
        meck:expect(avioneta_core, process_message, 2, ok),

        avioneta_handler:init([{worker, wk1}])
    end,
    fun(_) ->
        meck:unload([avioneta_channel, avioneta_core])
    end,
    fun(State) ->
        avioneta_handler:handle(message, State),
        [
          ?_assertEqual(meck:called(avioneta_core, process_message, [message, ch1]), true)
        ]
    end}.
