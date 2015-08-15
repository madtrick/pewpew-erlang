-module(pewpew_utils).

-export([
  proplist_to_map/1,
  get_value_in_map/2
]).

proplist_to_map(Proplist) ->
  lists:foldl(fun(Element, Map) ->
    {Key, Value} = Element,

    V = case is_list(Value) of
      true -> proplist_to_map(Value);
      false -> Value
    end,

    maps:put(Key, V, Map)
  end, #{}, Proplist).

get_value_in_map(Key, Map) when is_atom(Key) ->
  get_value_in_map([Key], Map);
get_value_in_map([], Result) ->
  Result;
get_value_in_map(Keys, Map) ->
  [Key | Tail] = Keys,
  NewMap = maps:get(Key, Map),
  get_value_in_map(Tail, NewMap).
