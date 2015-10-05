-module(pewpew_player_shoot_ack).

-export([
  new/2,
  toJSON/1
]).

new(Channel, ShootingInfo) ->
  Data = [{shooting_info, ShootingInfo}],
  pewpew_player_shoot_ack_data:new(?MODULE, Channel, Data).

toJSON(AckData) ->
  pewpew_player_shoot_ack_serializer:toJSON(AckData).
