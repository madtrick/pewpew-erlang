-module(pewpew_utils).
-include_lib("eunit/include/eunit.hrl").

-export([
  proplist_to_map/1,
  get_value_in_map/2,
  get_value_in_map/3,
  set_value_in_map/3,
  get_current_time_in_milliseconds/0,
  ceil/1
]).

proplist_to_map(Proplist) ->
  lists:foldl(fun(Element, Map) ->
    {Key, Value} = Element,
    maps:put(Key, Value, Map)
  end, #{}, Proplist).

get_value_in_map(KeyOrKeys, Map, Default) ->
  try get_value_in_map(KeyOrKeys, Map) catch _:bad_key -> Default end.
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

get_current_time_in_milliseconds() ->
  {Mega, Sec, Micro} = erlang:now(),
  MilliSeconds       = (Mega*1000000 + Sec)*1000 + round(Micro/1000),
  MilliSeconds.

ceil(X) when X < 0 ->
  trunc(X);
ceil(X) ->
  T = trunc(X),
  case X - T == 0 of
    true -> T;
    false -> T + 1
  end.
