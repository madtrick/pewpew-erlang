-module(pewpew_utils).

-export([
  proplist_to_map/1
]).

proplist_to_map(Proplist) ->
  lists:foldl(fun(Element, Map) ->
    {Key, Value} = Element,
    maps:put(Key, Value, Map)
  end, #{}, Proplist).
