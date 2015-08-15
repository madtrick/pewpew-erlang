-module(pewpew_config).

-export([load/0, init/1]).
-export([
  get/1,
  get/2,
  set/2
]).

-define(APPLICATION, pewpew).
-define(CONFIG_FILE, "pewpew.conf").

init(Config) when is_list(Config) ->
  ConfigMap = pewpew_utils:proplist_to_map(Config),
  application:set_env(?APPLICATION, config, ConfigMap).

load() ->
  {ok, Conf} = fserlangutils_app:read_in_priv(?APPLICATION, ?CONFIG_FILE),
  pewpew_config:init(proplists:get_value(fserlangutils_app:execution_mode(?APPLICATION), Conf)).

get(KeyOrKeys) ->
  {ok, Env}  = application:get_env(?APPLICATION, config),
  pewpew_utils:get_value_in_map(KeyOrKeys, Env).

get(Key, Default) ->
  case pewpew_config:get(Key) of
    [] -> Default;
    Value -> Value
  end.

set(Key, Value) ->
  {ok, Env} = application:get_env(?APPLICATION, config),
  NewEnv    = pewpew_utils:set_value_in_map(Key, Value, Env),
  application:set_env(?APPLICATION, config, NewEnv).
