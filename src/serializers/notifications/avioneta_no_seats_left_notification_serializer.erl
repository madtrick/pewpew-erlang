-module(avioneta_no_seats_left_notification_serializer).

-export([toJSON/1]).

toJSON(_NoSeatsLeftNotificationData) ->
  Struct = {[
      {type, <<"NoSeatsLeftNotification">>},
      {data, {[]}}
    ]},
  jiffy:encode(Struct).
