-module(avioneta_app).
-behaviour(application).

-export([start/2, stop/1]).

-define(APPLICATION, avioneta).
-define(CONFIG_FILE, "avioneta.conf").

start(_StartType, _StartArgs) ->
  lager:info("Execution mode ~s", [fserlangutils_app:execution_mode(?APPLICATION)]),

  init_random_seed(),
  init_config(),
  avioneta_sup:start_link().

stop(_State) ->
  ok.

init_random_seed() ->
  random:seed(erlang:now()).

init_config() ->
  {ok, Conf} = fserlangutils_app:read_in_priv(?APPLICATION, ?CONFIG_FILE),
  avioneta_config:init(proplists:get_value(fserlangutils_app:execution_mode(?APPLICATION), Conf)).

