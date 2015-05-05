-module(pewpew_no_seats_left_notification).

-export([new/0, toJSON/1]).

new() ->
  pewpew_no_seats_left_notification_data:new(?MODULE).

toJSON(NotificationData) ->
  pewpew_no_seats_left_notification_serializer:toJSON(NotificationData).
