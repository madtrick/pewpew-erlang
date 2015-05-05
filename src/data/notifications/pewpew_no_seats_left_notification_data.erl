-module(avioneta_no_seats_left_notification_data).

-export([new/1]).

-record(avioneta_no_seats_left_notification_data, {}).

new(Notification) ->
  NotificationData = #avioneta_no_seats_left_notification_data{},

  avioneta_message_data:new(Notification, NotificationData).
