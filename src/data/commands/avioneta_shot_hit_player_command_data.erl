-module(avioneta_shot_hit_player_command_data).

-export([new/2]).
-export([shot_id/1, player_id/1]).

-record(avioneta_shot_hit_player_command_data,{
    player_id,
    shot_id
  }).

new(Runner, Data) ->
  CommandData = #avioneta_shot_hit_player_command_data{
    player_id = proplists:get_value(player_id, Data),
    shot_id = proplists:get_value(shot_id, Data)
  },

  avioneta_command_data:new(Runner, CommandData).

player_id(#avioneta_shot_hit_player_command_data{ player_id = PlayerId}) -> PlayerId.
shot_id(#avioneta_shot_hit_player_command_data{ shot_id = ShotId }) -> ShotId.

