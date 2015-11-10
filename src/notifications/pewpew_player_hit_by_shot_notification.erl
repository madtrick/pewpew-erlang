-module(pewpew_player_hit_by_shot_notification).

-export([new/1]).

new(Player) ->
  Data = pewpew_dataset:new([{player, Player}]),
  pewpew_message:new(pewpew_player_hit_by_shot_notification_serializer, Data).
