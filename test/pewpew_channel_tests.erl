-module(pewpew_channel_tests).
-include_lib("eunit/include/eunit.hrl").

when_calling_exit_test() ->
  {ok, Pid} = pewpew_channel:create(worker),
  true = is_process_alive(Pid),
  pewpew_channel:exit(Pid),
  timer:sleep(1000),
  false = is_process_alive(Pid).

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
