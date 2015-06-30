-module(pewpew_radar_scan_notification).

-export([
  new/1,
  toJSON/1
]).

new(ScanData) ->
  pewpew_radar_scan_notification_data:new(?MODULE, ScanData).

toJSON(NotificationData) ->
  pewpew_radar_scan_notification_serializer:toJSON(NotificationData).
