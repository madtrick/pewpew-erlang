-module(pewpew_game_snapshot_notification).

-export([new/1]).

new(SnapshotData) ->
  pewpew_message:new(pewpew_game_snapshot_notification_serializer, SnapshotData).
