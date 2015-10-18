-module(pewpew_arena_component_snapshot).

-export([
  new/1
]).

new(ArenaComponentData) ->
  Players          = pewpew_arena_component_data:players(ArenaComponentData),
  PlayersSnapshots = lists:map(fun pewpew_player_component:snapshot/1, Players),
  Shots = pewpew_arena_component_data:shots(ArenaComponentData),
  ShotsSnapshots = lists:map(fun pewpew_shot_component:snapshot/1, Shots),

  #{
     players_snapshots => PlayersSnapshots,
     shots_snapshots => ShotsSnapshots
  }.
