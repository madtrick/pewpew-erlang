-module(pewpew_move_player_ack_data).

-export([
  new/3,
  coordinates/1,
  rotation/1
]).

new(AckModule, Channel, Data) ->
  AckData = pewpew_map_backed_data:new(Data),
  pewpew_message_data:new(AckModule, Channel, AckData).

coordinates(#{ coordinates := Value }) -> Value.
rotation(#{ rotation := Value }) -> Value.
