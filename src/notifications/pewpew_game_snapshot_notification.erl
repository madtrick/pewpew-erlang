-module(pewpew_game_snapshot_notification).

-export([
  new/1,
  toJSON/1
]).

new(SnapshotData) ->
  pewpew_game_snapshot_notification_data:new(?MODULE, SnapshotData).

toJSON(NotificationData) ->
  pewpew_game_snapshot_notification_serializer:toJSON(NotificationData).
