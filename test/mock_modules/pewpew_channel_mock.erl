-module(pewpew_channel_mock).

-export([start/0]).

start() ->
  spawn(
    fun() ->
      receive
        stop -> ok
      end
    end).
