-module(pewpew_player_hit_by_shot_notification_serializer).
-export([toJSON/1]).

toJSON(NotificationData) ->
  DataStruct = transform_data(NotificationData),

  Struct = #{
    type => <<"PlayerHitByShotNotification">>,
    data => DataStruct
  },

  Struct.

transform_data(NotificationData) ->
  PlayerData = pewpew_dataset:get(player, NotificationData),
  {life, PlayerLife} = PlayerData,

  #{
    life => PlayerLife
  }.
