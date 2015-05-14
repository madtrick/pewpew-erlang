-module(pewpew_register_player_ack_data).

-export([new/2]).
-export([player_component/1]).

-record(pewpew_register_player_ack_data, {
  player_component
}).

new(AckModule, Data) ->
  Channel = proplists:get_value(channel, Data),
  AckData = #pewpew_register_player_ack_data{
    player_component = proplists:get_value(player_component, Data)
  },
  pewpew_message_data:new(AckModule, Channel, AckData).

player_component(#pewpew_register_player_ack_data{ player_component = PlayerComponent }) -> PlayerComponent.
