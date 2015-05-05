-module(pewpew_no_seats_left_notification_data).

-export([new/1]).

-record(pewpew_no_seats_left_notification_data, {}).

new(Notification) ->
  NotificationData = #pewpew_no_seats_left_notification_data{},

  pewpew_message_data:new(Notification, NotificationData).
