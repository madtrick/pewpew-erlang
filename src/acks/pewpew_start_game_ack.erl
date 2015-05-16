-module(pewpew_start_game_ack).

-export([
  new/1,
  toJSON/1
]).

new(Channel) ->
  Data = [{channel, Channel}],
  pewpew_start_game_ack_data:new(?MODULE, Data).

toJSON(AckData) ->
  pewpew_start_game_ack_serializer:toJSON(AckData).
