-module(pewpew_utils).

-export([
  proplist_to_map/1,
  get_value_in_map/2,
  set_value_in_map/3
]).

proplist_to_map(Proplist) ->
  lists:foldl(fun(Element, Map) ->
    {Key, Value} = Element,
    maps:put(Key, Value, Map)
  end, #{}, Proplist).

get_value_in_map(Key, Map) when is_atom(Key) ->
  get_value_in_map([Key], Map);
get_value_in_map([], Result) ->
  Result;
get_value_in_map(Keys, Map) ->
  [Key | Tail] = Keys,
  NewMap = maps:get(Key, Map),
  get_value_in_map(Tail, NewMap).

set_value_in_map(Key, Value, Map) when is_atom(Key) ->
  set_value_in_map([Key], Value, Map);
set_value_in_map([Key], Value, Map) ->
  maps:put(Key, Value, Map);
set_value_in_map(Keys, Value, Map) ->
  [Key | Tail] = Keys,
  ValueForKey = maps:get(Key, Map, not_found),

  NewValueForKey = case is_map(ValueForKey) of
    true ->
      set_value_in_map(Tail, Value, ValueForKey);
    false ->
      set_value_in_map(Tail, Value, #{})
  end,

  maps:put(Key, NewValueForKey, Map).
