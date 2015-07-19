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
  #{scanned_players := ScannedPlayers} = ScanData,

  ScannedPlayersStruct = transform_data(players, ScannedPlayers, IsElementTypeReturned),

  #{elements => ScannedPlayersStruct, walls => []}.

transform_data(players, Players, false = _IsElementTypeReturned) ->
  lists:map(fun(Player) ->
    {x, X, y, Y} = pewpew_player_component:coordinates(Player),
    Coordinates = #{x => X, y=> Y},
    Type = unknown,

    #{coordinates => Coordinates, type => Type}
  end, Players).
