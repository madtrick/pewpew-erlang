-module(pewpew_player_hit_by_shot_notification).

-export([
  new/1,
  toJSON/1
]).

new(Player) ->
  pewpew_player_hit_by_shot_notification_data:new(?MODULE, #{player => Player}).

toJSON(NotificationData) ->
  pewpew_player_hit_by_shot_notification_serializer:toJSON(NotificationData).
