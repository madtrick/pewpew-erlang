-module(pewpew_radar_scan_notification_serializer).

-export([toJSON/1]).

toJSON(ScanData) ->
  #{scanned_players := Players, scanned_walls := Walls} = ScanData,

  Struct = #{
    type => <<"RadarScanNotification">>,
    data => #{
      players => Players,
      walls => Walls
     }
   },

  Struct.
