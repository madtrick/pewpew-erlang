-module(pewpew_utils).
-include_lib("eunit/include/eunit.hrl").

-export([
  proplist_to_map/1,
  get_value_in_map/2,
  get_value_in_map/3,
  set_value_in_map/3,
  get_current_time_in_milliseconds/0,
  ceil/1,
  circles_intersect/2,
  contact_points_between_segment_and_circle/2,
  contact_points_between_line_and_circle/2,
  degrees_to_radians/1,
  round_value/2
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

contact_points_between_segment_and_circle(
  [{x, X, y, Y1}, {x, X, y, Y2}],
  Circle
) ->
  Line   = {x, X, y, Y1, rotation, 90},
  Points = contact_points_between_line_and_circle(Line, Circle),
  [Point || Point <- Points, check_if_point_belongs_to_segment(y, Point, Y1, Y2)];
contact_points_between_segment_and_circle(Segment, Circle) ->
  [{x, X1, y, Y1}, {x, X2, y, Y2}] = Segment,

  Slope          = abs((Y1 - Y2) / (X1 - X2)),
  AngleInRadians = math:atan(Slope),
  AngleInDegrees = (180 * AngleInRadians) / math:pi(),
  Line           = {x, X1, y, Y1, rotation, AngleInDegrees},
  Points         = contact_points_between_line_and_circle(Line, Circle),

  [Point || Point <- Points, check_if_point_belongs_to_segment(x, Point, X1, X2)].

contact_points_between_line_and_circle(
  {x, X, y, Y, rotation, Rotation},
  {x, Cx, y, Cy, radius, Cr}
 ) when Rotation =:= 90; Rotation =:= 270 ->
  % Invert the coordinates to transform the vertical line
  % into a horizontal one
  InvertedLine = {x, Y, y, X, rotation, 0},
  InvertedCircle = {x, Cy, y, Cx, radius, Cr},

  Results = contact_points_between_line_and_circle(InvertedLine, InvertedCircle),
  lists:map(fun (Result) ->
                {x, X1, y, Y1} = Result,
                {x, Y1, y, X1}
            end, Results);
%
% We substitute the equation of the
% line (y = mx + f) on the circle equation
%
% Circle equation:
%
%   (x−cx)^2+(y−cy)^2=r^2
%
% Substituting y:
%
%   (x - cx)^2 + (y - y1)^2 = r^2
%   (x - cx)^2 + (mx1 + f − cy)^2 = r^2.
%
% Expanding:
%
%  (m^2 + 1)x^2 + 2(mf − mcy − cx)x + (cy^2 − r^2 + p^2 − 2fcy + f^2) = 0
%
% A = (m^2 + 1)
% B = 2(mf − mcy − cx)
% C = cy^2 − r^2 + p^2 − 2fcy + f^2
%
contact_points_between_line_and_circle(Line, Circle) ->
  {x, X, y, Y, rotation, Rotation} = Line,
  {x, CX, y, CY, radius, CR} = Circle,

  M = math:tan(degrees_to_radians(Rotation)),
  F = -M*X + Y,

  A = math:pow(M, 2) + 1,
  B = 2 * (M*F - M*CY - CX),
  C = math:pow(CY, 2) - math:pow(CR, 2) + math:pow(CX, 2) - 2*F*CY + math:pow(F, 2),

  case cuadratic_equation(A, B, C) of
    no_solution -> [];
    {val1, Val1, val2, Val2} ->
      NewY1 = M*Val1 + F,
      NewY2 = M*Val2 + F,

      case Val1 =:= Val2 of
        true -> % tangential
          [
           {x, round_value(Val1, 5), y, round_value(NewY1, 5)}
          ];
        false -> % intersection
          [
           {x, round_value(Val1, 5), y, round_value(NewY1, 5)},
           {x, round_value(Val2, 5), y, round_value(NewY2, 5)}
          ]
      end
  end.

degrees_to_radians(Degrees) ->
  math:pi() * Degrees / 180.

round_value(Value, Precision) ->
  P = math:pow(10, Precision),
  round(Value * P) / P.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cuadratic_equation(A, B, C) ->
  B_2 = math:pow(B, 2),
  Discriminant = B_2 - 4 * A * C,

  case Discriminant < 0 of
    true ->
      no_solution;
    false ->
      Temp = math:sqrt(Discriminant),
      Val1 = (-B - Temp) / (2*A),
      Val2 = (-B + Temp) / (2*A),

      {val1, Val1, val2, Val2}
  end.

check_if_point_belongs_to_segment(x, {x, X, _, _}, SegmentEnd1, SegmentEnd2) ->
  is_point_value_between_segment_ends(X, SegmentEnd1, SegmentEnd2);
check_if_point_belongs_to_segment(y, {_, _, y, Y}, SegmentEnd1, SegmentEnd2) ->
  is_point_value_between_segment_ends(Y, SegmentEnd1, SegmentEnd2).

is_point_value_between_segment_ends(PointValue, SegmentEnd1, SegmentEnd2) when SegmentEnd1 < SegmentEnd2 ->
  SegmentEnd1 =< PointValue andalso PointValue =< SegmentEnd2;
is_point_value_between_segment_ends(PointValue, SegmentEnd1, SegmentEnd2) when SegmentEnd1 > SegmentEnd2 ->
  SegmentEnd2 =< PointValue andalso PointValue =< SegmentEnd1.
