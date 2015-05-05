-module(pewpew).

-export([start/0, stop/0]).

start() ->
  ok = fserlangutils_app:ensure_started(lager),
  application:start(pewpew).

stop() ->
  application:stop(pewpew).
