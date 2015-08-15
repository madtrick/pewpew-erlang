-module(pewpew_utils).

-export([
  proplist_to_map/1
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
