-module(pewpew_register_player_ack_serializer).

-export([toJSON/1]).

toJSON(RegisterPlayerAckData) ->
  Player = pewpew_dataset:get(player_component, RegisterPlayerAckData),

  #{
    type => <<"RegisterPlayerAck">>,
    data =>#{
      id => pewpew_player_component:id(Player),
      x => pewpew_player_component:x(Player),
      y => pewpew_player_component:y(Player),
      life => pewpew_player_component:life(Player),
      shooting => pewpew_player_component:shooting_info(Player)
      }
    }.
