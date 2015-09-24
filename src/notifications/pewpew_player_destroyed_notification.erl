-module(pewpew_player_destroyed_notification).

-export([
  new/0,
  toJSON/1
]).

new() ->
  pewpew_player_destroyed_notification_data:new(?MODULE).

toJSON(NotificationData) ->
  pewpew_player_destroyed_notification_serializer:toJSON(NotificationData).

