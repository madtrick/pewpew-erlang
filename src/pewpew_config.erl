-module(pewpew_config).

-export([load/0, init/1]).
-export([
  get/1,
  get/2,
  set/2
]).

-define(APPLICATION, pewpew).
-define(CONFIG_FILE, "pewpew.conf").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Config) ->
  Map = init_map_from_proplist(Config, #{}),
  application:set_env(?APPLICATION, config, Map).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_map_from_proplist([], Map) ->
  Map;
init_map_from_proplist([{Key, Value} | Tail], Map) ->
  NewMap = case is_proplist(Value) of
    true ->
      maps:put(Key, init_map_from_proplist(Value, #{}), Map);
    false ->
      maps:put(Key, Value, Map)
  end,

  init_map_from_proplist(Tail, NewMap).

is_proplist(Value) when not is_list(Value) ->
  false;
is_proplist(Value) ->
  lists:all(fun(E) ->
    is_tuple(E) andalso tuple_size(E) =:= 2
  end, Value).
