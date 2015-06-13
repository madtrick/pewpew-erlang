-module(pewpew_app).
-behaviour(application).

-export([start/2, stop/1]).

-define(APPLICATION, pewpew).

start(_StartType, _StartArgs) ->
  lager:info("Execution mode ~s", [fserlangutils_app:execution_mode(?APPLICATION)]),

  init_random_seed(),
  pewpew_config:load(),
  pewpew_sup:start_link().

stop(_State) ->
  ok.

init_random_seed() ->
  random:seed(erlang:now()).
