-module(avioneta_config).

-export([init/1]).
-export([get/1, get/2]).

-define(APPLICATION, avioneta).

init(Config) ->
  application:set_env(?APPLICATION, config, Config).

get(Key) ->
  {ok, Env}  = application:get_env(?APPLICATION, config),
  kvc:path(Key, Env).

get(Key, Default) ->
  case avioneta_config:get(Key) of
    [] -> Default;
    Value -> Value
  end.
