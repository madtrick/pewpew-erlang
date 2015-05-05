-module(avioneta).

-export([start/0, stop/0]).

start() ->
  ok = fserlangutils_app:ensure_started(lager),
  application:start(avioneta).

stop() ->
  application:stop(avioneta).
