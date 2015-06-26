-module(pewpew_game_snapshot_notification_serializer).

-export([toJSON/1]).

toJSON(NotificationData) ->
  Struct = #{
    type => <<"GameSnapshotNotification">>,
    data => NotificationData
   },
  Struct.
