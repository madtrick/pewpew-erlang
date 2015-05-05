-module(avioneta_channel_tests).
-include_lib("eunit/include/eunit.hrl").

when_created_test_() ->
  [
    it_registers_it_self_test()
  ].

it_registers_it_self_test() ->
  {setup,
    fun() ->
      meck:new(avioneta_registry),
      meck:expect(avioneta_registry, register, 1, ok)
    end,
    fun(_) ->
      meck:unload(avioneta_registry)
    end,
    fun(_) ->
      avioneta_channel:create(wsworker),
      [
        ?_assertEqual(meck:called(avioneta_registry, register, ['_']), true)
      ]
    end}.

it_sends_given_data_test_() ->
  {setup,
    fun () ->
        meck:new(wsworker),
        meck:expect(wsworker, send, 2, ok),
        {ok, Pid} = avioneta_channel:create(worker),
        Pid
    end,
    fun (_) ->
        meck:unload(wsworker)
    end,
    fun (Pid) ->
        avioneta_channel:send(Pid, data),
        timer:sleep(100),
        [
          ?_assertEqual(meck:called(wsworker, send, [worker, data]), true)
        ]
    end}.
