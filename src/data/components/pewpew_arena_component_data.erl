-module(pewpew_arena_component_data).

-export([new/1]).
-export([
  players/1,
  shots/1,
  pewpew_player_component_sup/1,
  shot_component_sup/1,
  max_number_of_players/1,
  width/1,
  height/1,
  radar_component/1
]).
-export([update/2]).

new(Data) ->
  Options = [{players, []}, {shots, []} | Data],
  pewpew_map_backed_data:new(Options).

pewpew_player_component_sup(#{ pewpew_player_component_sup := Value }) -> Value.
shot_component_sup(#{ shot_component_sup := Value }) -> Value.
max_number_of_players(#{ max_number_of_players := Value }) -> Value.
players(#{ players := Value }) -> Value.
shots(#{ shots := Value }) -> Value.
width(#{ width := Value }) -> Value.
height(#{ height := Value }) -> Value.
radar_component(#{ radar_component := Value }) -> Value.

update(ArenaComponentData, Options) ->
  pewpew_map_backed_data:update(ArenaComponentData, Options).
