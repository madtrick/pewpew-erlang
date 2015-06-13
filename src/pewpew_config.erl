-module(pewpew_config).

-export([load/0, init/1]).
-export([get/1, get/2]).

-define(APPLICATION, pewpew).
-define(CONFIG_FILE, "pewpew.conf").

init(Config) ->
  application:set_env(?APPLICATION, config, Config).

load() ->
  {ok, Conf} = fserlangutils_app:read_in_priv(?APPLICATION, ?CONFIG_FILE),
  pewpew_config:init(proplists:get_value(fserlangutils_app:execution_mode(?APPLICATION), Conf)).

get(Key) ->
  {ok, Env}  = application:get_env(?APPLICATION, config),
  kvc:path(Key, Env).

get(Key, Default) ->
  case pewpew_config:get(Key) of
    [] -> Default;
    Value -> Value
  end.
