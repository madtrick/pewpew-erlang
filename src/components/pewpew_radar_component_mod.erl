-module(pewpew_radar_component_mod).
-include_lib("eunit/include/eunit.hrl").

-export([
  change_radar_mode/2,
  circular_scan/3,
  long_range_scan/3
]).

-define(RADAR_MODES, #{<<"long_range_scan">> => #{radius => 80}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

change_radar_mode(NewMode, RadarConfigData) ->
  IsValidRadarMode = maps:is_key(NewMode, ?RADAR_MODES),

  case IsValidRadarMode of
    true ->
      NewModeConfig               = maps:get(NewMode, ?RADAR_MODES),
      #{radius := NewModeRadius}  = NewModeConfig,
      ValuesToUpdate              = [{mode, NewMode}, {radius, NewModeRadius}],
      UpdatedRadarConfigData      = pewpew_radar_config_data:update(RadarConfigData, ValuesToUpdate),

      {ok, UpdatedRadarConfigData};
    false -> {error, RadarConfigData}
  end.

circular_scan(ArenaDimensions, Players, ScanningPlayer) ->
  ScanRadius = 40,
  {width, ArenaWidth, height, ArenaHeight} = ArenaDimensions,
  PlayersToCheck = [Player || Player <- Players, Player =/= ScanningPlayer],

  PlayersUnderRadar = players_under_radar(PlayersToCheck, ScanningPlayer, ScanRadius),
  Walls = intersections_with_walls(ArenaHeight, ArenaWidth, ScanningPlayer, ScanRadius),

  #{
    players => PlayersUnderRadar,
    walls => Walls
  }.

long_range_scan(_ArenaDimensions, Players, ScanningPlayer) ->
  ScanRadius = 80,
  PlayersToCheck    = [Player || Player <- Players, Player =/= ScanningPlayer],
  PlayersUnderRadar = players_under_radar(PlayersToCheck, ScanningPlayer, ScanRadius),

  % TODO add more tests for this method
  % TODO refactor this
  {x, ScanningPlayerX, y, ScanningPlayerY} = pewpew_player_component:coordinates(ScanningPlayer),
  ScanningPlayerRotation                   = pewpew_player_component:rotation(ScanningPlayer),

  PlayersUnderLongRangeRadar = lists:filter(
    fun(Player) ->
        {x, X, y, Y} = pewpew_player_component:coordinates(Player),

        case X - ScanningPlayerX of
          0 -> Y > ScanningPlayerY;
          _ ->
            LeftBoundsSlope  = (math:tan(math:pi() / 6 + ScanningPlayerRotation)),
            RightBoundsSlope = (math:tan(ScanningPlayerRotation - math:pi() / 6)),


            %
            % X1, Y1 coordinates of scanned player
            % X, Y coordiantes of scanning player

            %             |
            %     X1 < X  | X1 > X
            %     Y1 > Y  | Y1 > Y
            %             |
            %   ---------------------
            %             |
            %     X1 < X  | X1 > X
            %     Y1 < Y  | Y1 < Y
            %             |
            %

            Base    = erlang:abs(X - ScanningPlayerX),
            Height  = erlang:abs(Y - ScanningPlayerY),
            Tangent = Height / Base,

            Angle = case {X < ScanningPlayerX, Y < ScanningPlayerY} of
              {true, true} -> math:pi() + Tangent;
              {true, false} -> math:pi() - Tangent;
              {false, false} -> Tangent;
              {false, true} -> math:pi() * (3/2) +  Tangent
            end,

            Angle >= RightBoundsSlope andalso Angle =< LeftBoundsSlope
        end
    end,
    PlayersUnderRadar
   ),

  #{
    players => PlayersUnderLongRangeRadar,
    walls => []
  }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

players_under_radar([], _, _) ->
  [];
players_under_radar(Players, ScanningPlayer, ScanRadius) ->
  {x, ScanningPlayerX, y, ScanningPlayerY} = pewpew_player_component:coordinates(ScanningPlayer),

  lists:filter(
    fun(Player) ->
        {x, X, y, Y} = pewpew_player_component:coordinates(Player),

        A = erlang:abs(X - ScanningPlayerX),
        B = erlang:abs(Y - ScanningPlayerY),
        Distance = math:sqrt(A*A + B*B),

        Distance =< ScanRadius
    end,
   Players).

intersections_with_walls(ArenaHeight, ArenaWidth, ScanningPlayer, ScanRadius) ->
  Intersections = [
   intersect_right_wall(ArenaHeight, ArenaWidth, ScanningPlayer, ScanRadius),
   intersect_bottom_wall(ArenaHeight, ArenaWidth, ScanningPlayer, ScanRadius),
   intersect_left_wall(ArenaHeight, ArenaWidth, ScanningPlayer, ScanRadius),
   intersect_top_wall(ArenaHeight, ArenaWidth, ScanningPlayer, ScanRadius)
  ],

  [ Intersection || Intersection <- Intersections, Intersection =/= []].

intersect_left_wall(ArenaHeight, _ArenaWidth, ScanningPlayer, ScanRadius) ->
  LeftWallLine = 0,
  intersect_with_line(vertical, ArenaHeight, 0, LeftWallLine, ScanningPlayer, ScanRadius).
intersect_right_wall(ArenaHeight, ArenaWidth, ScanningPlayer, ScanRadius) ->
  RightWallLine = ArenaWidth,
  intersect_with_line(vertical, ArenaHeight, 0, RightWallLine, ScanningPlayer, ScanRadius).
intersect_bottom_wall(_, ArenaWidth, ScanningPlayer, ScanRadius) ->
  BottomWallLine = 0,
  intersect_with_line(horizontal, ArenaWidth, 0, BottomWallLine, ScanningPlayer, ScanRadius).
intersect_top_wall(ArenaHeight, ArenaWidth, ScanningPlayer, ScanRadius) ->
  TopWallLine = ArenaHeight,
  intersect_with_line(horizontal, ArenaWidth, 0, TopWallLine, ScanningPlayer, ScanRadius).

intersect_with_line(
  WallType,
  MaxIntersectionPoint,
  MinIntersectionPoint,
  WallLine,
  ScanningPlayer,
  ScanRadius
) ->
  {x, PlayerX, y, PlayerY} = pewpew_player_component:coordinates(ScanningPlayer),
  calculate_coordinates(WallType, MaxIntersectionPoint, MinIntersectionPoint, WallLine, PlayerX, PlayerY, ScanRadius).

% (C1, C2) = center of the player
% r = radius of the scan
%
% z^2 + y^2 -2*C1*z - 2*C2*y + C1^2 + C2^2 = r^2
%
%
% a*y^2 + b*y + c, where
%
% a = 1
% b = -2*C2
% c = z^2 - 2*C1*z + C1^2 + C2^2 - R^2
%
% y = (-b +- sqrt(b^2 - 4*a*c))/2*a
%
% Tangent if b^2 == 4*a*c
% Intersection if b^2 > 4*a*c
% No intersection in the other case
%
calculate_coordinates(
  LineType,
  MaxIntersectionPoint,
  MinIntersectionPoint,
  WallLine,
  PlayerX, PlayerY,
  ScanRadius
) ->
  {C1, C2} = case LineType of
               vertical -> {PlayerY, PlayerX};
               horizontal -> {PlayerX, PlayerY}
             end,

  A = 1,
  B = -2 * C1,
  C = (WallLine * WallLine) - 2 * C2 * WallLine + (C1 * C1) + (C2 * C2) - (ScanRadius * ScanRadius),

  FourAC  = 4 * A * C,
  SquareB = math:pow(B, 2),
  Temp    = SquareB - FourAC,

  case Temp > 0 of
    true -> % intersection

      P_1 = erlang:trunc((-B + math:sqrt(Temp)) / 2 * A),
      P_2 = erlang:trunc((-B - math:sqrt(Temp)) / 2 * A),

      NormalizedP_1 = normalize_intersection_point(gt, P_1, MaxIntersectionPoint),
      NormalizedP_2 = normalize_intersection_point(lt, P_2, MinIntersectionPoint),

      case LineType of
        vertical ->
          [{WallLine, NormalizedP_1}, {WallLine, NormalizedP_2}];
        horizontal ->
          [{NormalizedP_1, WallLine}, {NormalizedP_2, WallLine}]
      end;
    false ->
      case Temp =:= 0.0 of
        true -> % tangential
          P_1 = erlang:trunc(-B / 2 * A),

          case LineType of
            vertical ->
              [{WallLine, P_1}];
            horizontal ->
              [{P_1, WallLine}]
          end;
        _ -> % no intersection
          []
      end
  end.

normalize_intersection_point(gt, Point, Max) when Point > Max -> Max;
normalize_intersection_point(gt, Point, _) -> Point;
normalize_intersection_point(lt, Point, Min) when Point < Min -> Min;
normalize_intersection_point(lt, Point, _) -> Point.
