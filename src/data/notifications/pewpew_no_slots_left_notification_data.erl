-module(pewpew_no_slots_left_notification_data).

-export([new/1]).

new(Notification) ->
  pewpew_message_data:new(Notification, undefined).
