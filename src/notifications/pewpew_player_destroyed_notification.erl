-module(pewpew_player_destroyed_notification).

-export([new/0]).

new() ->
  pewpew_message:new(pewpew_player_destroyed_notification_serializer).
