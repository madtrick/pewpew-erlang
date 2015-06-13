-module(pewpew_game_snapshot_notification_data).

-export([new/2]).

new(Notification, Data) ->
  pewpew_message_data:new(Notification, Data).
