-module(pewpew_arena_component_snapshot).

-export([
  new/1
]).

new(ArenaComponentData) ->
  Players          = pewpew_arena_component_data:players(ArenaComponentData),
  PlayersSnapshots = lists:map(fun pewpew_player_component:snapshot/1, Players),

  #{
     players_snapshots => PlayersSnapshots
  }.
