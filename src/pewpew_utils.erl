-module(pewpew_utils).
-include_lib("eunit/include/eunit.hrl").

-export([
  proplist_to_map/1,
  get_value_in_map/2,
  get_value_in_map/3,
  set_value_in_map/3,
  get_current_time_in_milliseconds/0,
  ceil/1,
  circles_intersect/2
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

circles_intersect(Circle1, Circle2) ->
  {x, C1_x, y, C1_y, radius, C1_r} = Circle1,
  {x, C2_x, y, C2_y, radius, C2_r} = Circle2,

  % collision detection formula got at:
  % http://stackoverflow.com/a/8367547/1078859
  Constant = math:pow(C1_x - C2_x, 2) + math:pow(C1_y - C2_y, 2),
  Collides = math:pow(C1_r - C2_r, 2) >= Constant orelse
             Constant =< math:pow(C1_r + C2_r, 2),

  Collides.
