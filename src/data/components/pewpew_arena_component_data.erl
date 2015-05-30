-module(pewpew_arena_component_data).

-export([new/1]).
-export([
  players/1,
  pewpew_player_component_sup/1,
  pewpew_game_context_data/1,
  max_number_of_players/1,
  width/1,
  height/1
]).
-export([update/2]).

new(Data) ->
  Options = [{players, []} | Data],
  pewpew_map_backed_data:new(Options).

pewpew_player_component_sup(#{ pewpew_player_component_sup := Value }) -> Value.
pewpew_game_context_data(#{ pewpew_game_context_data := Value }) -> Value.
max_number_of_players(#{ max_number_of_players := Value }) -> Value.
players(#{ players := Value }) -> Value.
width(#{ width := Value }) -> Value.
height(#{ height := Value }) -> Value.

update(ArenaComponentData, Options) ->
  pewpew_map_backed_data:update(ArenaComponentData, Options).
