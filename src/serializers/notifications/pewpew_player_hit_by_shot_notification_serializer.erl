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
  #{player := PlayerData} = NotificationData,
  {life, PlayerLife} = PlayerData,

  #{
    life => PlayerLife
  }.
