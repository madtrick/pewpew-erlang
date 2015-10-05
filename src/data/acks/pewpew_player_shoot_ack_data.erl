-module(pewpew_player_shoot_ack_data).

-export([
  new/3,
  shooting_info/1
  ]).

new(AckModule, Channel, AckData) ->
  Data = pewpew_map_backed_data:new(AckData),
  pewpew_message_data:new(AckModule, Channel, Data).

shooting_info(#{ shooting_info := Value }) -> Value.
