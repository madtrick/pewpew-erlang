-module(pewpew_arena_component_mod).

-export([
  get_player/2,
  move_player/3
]).

get_player(Condition, ArenaComponentData) ->
  MatchingPlayers = find_players_matching_condition(Condition, ArenaComponentData),

  case MatchingPlayers of
    []       -> {ok, undefined};
    [Player] -> {ok, Player}
  end.

move_player(Player, Movement, _) ->
  pewpew_player_component:move(Player, Movement),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

find_players_matching_condition(Condition, ArenaComponentData) ->
  [
   Player ||
   Player <- pewpew_arena_component_data:players(ArenaComponentData),
   (pewpew_player_component:id(Player) =:= Condition) or
   (pewpew_player_component:channel(Player) =:= Condition)
  ].
