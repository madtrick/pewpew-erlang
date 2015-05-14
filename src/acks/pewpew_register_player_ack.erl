-module(pewpew_register_player_ack).

-export([new/2, toJSON/1]).

new(PlayerComponent, Channel) ->
  Values = [
    {channel, Channel},
    {player_component, PlayerComponent}
  ],
  pewpew_register_player_ack_data:new(?MODULE, Values).

toJSON(AckData) ->
  pewpew_register_player_ack_serializer:toJSON(AckData).
