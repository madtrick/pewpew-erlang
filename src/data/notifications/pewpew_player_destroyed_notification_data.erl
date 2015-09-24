-module(pewpew_player_destroyed_notification_data).

-export([new/1]).

new(Notification) ->
  pewpew_message_data:new(Notification, []).
