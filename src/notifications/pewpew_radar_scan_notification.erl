-module(pewpew_radar_scan_notification).

-export([new/1]).

new(ScanData) ->
  pewpew_message:new(pewpew_radar_scan_notification_serializer, ScanData).
