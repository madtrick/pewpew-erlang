-module(pewpew_radar_scan_notification_serializer).

-include_lib("eunit/include/eunit.hrl").
-export([toJSON/1]).

toJSON(ScanData) ->
  DataStruct = transform_data(ScanData, false),

  Struct = #{
    type => <<"RadarScanNotification">>,
    data => DataStruct
  },

  Struct.

transform_data(ScanData, IsElementTypeReturned) ->
  #{
    scanned_players := ScannedPlayers,
    scanned_walls := ScannedWalls
  } = ScanData,

  ScannedPlayersStruct = transform_data(players, ScannedPlayers, IsElementTypeReturned),
  ScannedWallsStruct   = transform_data(walls, ScannedWalls, false),

  #{elements => ScannedPlayersStruct, walls => ScannedWallsStruct}.

transform_data(walls, Walls, _) ->
  lists:map(fun(Wall) ->
                ?debugVal(Wall),
    [[X, Y] || {X, Y} <- Wall]
  end, Walls);
transform_data(players, Players, false = _IsElementTypeReturned) ->
  lists:map(fun(Player) ->
    {x, X, y, Y} = pewpew_player_component:coordinates(Player),
    Coordinates = #{x => X, y=> Y},
    Type = unknown,

    #{coordinates => Coordinates, type => Type}
  end, Players).
