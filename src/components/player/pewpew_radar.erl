-module(pewpew_radar).
-include_lib("eunit/include/eunit.hrl").

-export([scan/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

scan(ArenaComponent, ScanningPlayer, ScanRadius) ->
  {width, ArenaWidth, height, ArenaHeight} = pewpew_arena_component:dimensions(ArenaComponent),
  Players     = pewpew_arena_component:players(ArenaComponent),
  PlayersToCheck = [Player || Player <- Players, Player =/= ScanningPlayer],

  PlayersUnderRadar = players_under_radar(PlayersToCheck, ScanningPlayer, ScanRadius),
  Walls = intersections_with_walls(ArenaHeight, ArenaWidth, ScanningPlayer, ScanRadius),

  #{
    players => PlayersUnderRadar,
    walls => Walls
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
  lists:flatten([
                 intersect_left_wall(ArenaHeight, ArenaWidth, ScanningPlayer, ScanRadius),
                 intersect_right_wall(ArenaHeight, ArenaWidth, ScanningPlayer, ScanRadius),
                 intersect_bottom_wall(ArenaHeight, ArenaWidth, ScanningPlayer, ScanRadius),
                 intersect_top_wall(ArenaHeight, ArenaWidth, ScanningPlayer, ScanRadius)
                ]
   ).

intersect_left_wall(_, ArenaWidth, ScanningPlayer, ScanRadius) ->
  LeftWallLine = ArenaWidth,
  intersect_with_line(vertical, LeftWallLine, ScanningPlayer, ScanRadius).
intersect_right_wall(_, _, ScanningPlayer, ScanRadius) ->
  RightWallLine = 0,
  intersect_with_line(vertical, RightWallLine, ScanningPlayer, ScanRadius).
intersect_bottom_wall(_, _, ScanningPlayer, ScanRadius) ->
  BottomWallLine = 0,
  intersect_with_line(horizontal, BottomWallLine, ScanningPlayer, ScanRadius).
intersect_top_wall(ArenaHeight, _, ScanningPlayer, ScanRadius) ->
  TopWallLine = ArenaHeight,
  intersect_with_line(horizontal, TopWallLine, ScanningPlayer, ScanRadius).

intersect_with_line(WallType, WallLine, ScanningPlayer, ScanRadius) ->
  {x, PlayerX, y, PlayerY} = pewpew_player_component:coordinates(ScanningPlayer),
  calculate_coordinates(WallType, WallLine, PlayerX, PlayerY, ScanRadius).

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
calculate_coordinates(LineType, Z, TC1, TC2, R) ->
  {C1, C2} = case LineType of
               horizontal -> {TC2, TC1};
               vertical -> {TC1, TC2}
             end,

  A = 1,
  B = -2 * C1,
  C = (Z * Z) - 2 * C2 * Z + (C1 * C1) + (C2 * C2) - (R * R),

  FourAC  = 4 * A * C,
  SquareB = math:pow(B, 2),
  Temp    = SquareB - FourAC,

  case Temp > 0 of
    true -> % intersection

      P_1 = erlang:trunc((-B + math:sqrt(Temp)) / 2 * A),
      P_2 = erlang:trunc((-B - math:sqrt(Temp)) / 2 * A),

      case LineType of
        horizontal ->
          [{Z, P_1}, {Z, P_2}];
        vertical ->
          [{P_1, Z}, {P_2, Z}]
      end;
    false ->
      case Temp =:= 0.0 of
        true -> % tangential
          P_1 = erlang:trunc(-B / 2 * A),

          case LineType of
            horizontal ->
              [{Z, P_1}];
            vertical ->
              [{P_1, Z}]
          end;
        _ -> % no intersection
          []
      end
  end.
