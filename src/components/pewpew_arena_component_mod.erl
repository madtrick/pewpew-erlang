-module(pewpew_arena_component_mod).

-export([
  get_player/2,
  move_player/3,
  dimensions/1,
  update/1
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

dimensions(ArenaComponentData) ->
  Width  = pewpew_arena_component_data:width(ArenaComponentData),
  Height = pewpew_arena_component_data:height(ArenaComponentData),

  {ok, {width, Width, height, Height}}.

update(ArenaComponentData) ->
  Players = pewpew_arena_component_data:players(ArenaComponentData),
  PlayersUpdates = lists:map(fun(Player) ->
    {player, Player, update, pewpew_player_component:update(Player) }
  end, Players),

  {ok, PlayersUpdates}.

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
