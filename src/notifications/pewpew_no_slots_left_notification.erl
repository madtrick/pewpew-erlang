-module(pewpew_no_slots_left_notification).

-export([new/0]).

new() ->
  pewpew_message:new(pewpew_no_slots_left_notification_serializer).
