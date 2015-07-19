-module(pewpew_radar_scan_notification_serializer).

-include_lib("eunit/include/eunit.hrl").
-export([toJSON/1]).

toJSON(ScanData) ->
  #{scanned_players := Players, scanned_walls := Walls} = ScanData,

  Ps = lists:map(fun(P) ->
                     {x, X, y, Y} = pewpew_player_component:coordinates(P),
                     #{x => X, y=> Y}
                 end, Players),

  Struct = #{
    type => <<"RadarScanNotification">>,
    data => #{
      players => Ps,
      walls => Walls
     }
   },

  ?debugVal(Struct),


  Struct.

