-module(pewpew_no_slots_left_notification_serializer).

-export([toJSON/1]).

toJSON(_NoslotsLeftNotificationData) ->
  Struct = #{
      type => <<"NoSlotsLeftNotification">>,
      data => #{}
    },
  Struct.
