-module(pewpew_channel_tests).
-include_lib("eunit/include/eunit.hrl").

when_calling_exit_test() ->
  {ok, Pid} = pewpew_channel:create(worker),
  true = is_process_alive(Pid),
  pewpew_channel:exit(Pid),
  timer:sleep(1000),
  false = is_process_alive(Pid).

when_created_test_() ->
  [
    it_registers_it_self_test()
  ].

it_registers_it_self_test() ->
  {setup,
    fun() ->
      meck:new(pewpew_registry),
      meck:expect(pewpew_registry, register, 1, ok)
    end,
    fun(_) ->
      meck:unload(pewpew_registry)
    end,
    fun(_) ->
      pewpew_channel:create(wsworker),
      [
        ?_assertEqual(meck:called(pewpew_registry, register, ['_']), true)
      ]
    end}.

it_sends_given_data_test_() ->
  {setup,
    fun () ->
        meck:new(wsserver_worker_websocket),
        meck:expect(wsserver_worker_websocket, send, 2, ok),
        {ok, Pid} = pewpew_channel:create(worker),
        Pid
    end,
    fun (_) ->
        meck:unload(wsserver_worker_websocket)
    end,
    fun (Pid) ->
        pewpew_channel:send(Pid, data),
        timer:sleep(100),
        [
          ?_assertEqual(meck:called(wsserver_worker_websocket, send, [worker, data]), true)
        ]
    end}.
