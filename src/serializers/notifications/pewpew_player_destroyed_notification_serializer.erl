-module(pewpew_player_destroyed_notification_serializer).
-export([toJSON/1]).

toJSON(_) ->
  Struct = #{
    type => <<"PlayerDestroyedNotification">>,
    data => #{}
  },

  Struct.
