-module(pewpew_arena_component_data).

-export([new/1]).
-export([players/1, pewpew_player_component_sup/1, pewpew_game_context_data/1, max_number_of_players/1, width/1, height/1]).
-export([update/2]).

-record(pewpew_arena_component_data, {
    pewpew_player_component_sup,
    pewpew_game_context_data,
    max_number_of_players,
    players,
    width,
    height
  }).

new(Options) ->
  #pewpew_arena_component_data{
    pewpew_player_component_sup = proplists:get_value(pewpew_player_component_sup, Options),
    pewpew_game_context_data    = proplists:get_value(pewpew_game_context_data, Options),
    players                       = [],
    max_number_of_players         = proplists:get_value(max_number_of_players, Options),
    width                         = proplists:get_value(width, Options),
    height                        = proplists:get_value(height, Options)
  }.

pewpew_player_component_sup(#pewpew_arena_component_data{ pewpew_player_component_sup = PewpewPlayerComponentSup }) -> PewpewPlayerComponentSup.
pewpew_game_context_data(#pewpew_arena_component_data{ pewpew_game_context_data = PewpewGameContextData }) -> PewpewGameContextData.
max_number_of_players(#pewpew_arena_component_data{ max_number_of_players = MaxNumberOfPlayers }) -> MaxNumberOfPlayers.
players(#pewpew_arena_component_data{ players = Players }) -> Players.
width(#pewpew_arena_component_data{ width = Width }) -> Width.
height(#pewpew_arena_component_data{ height = Height }) -> Height.

update(ArenaComponentData, Options) ->
  ArenaComponentData#pewpew_arena_component_data{
    players = proplists:get_value(players, Options, players(ArenaComponentData))
  }.
