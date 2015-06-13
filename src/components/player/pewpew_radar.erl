-module(pewpew_radar).

-export([scan/3]).

scan(ArenaComponent, ScanningPlayer, ScanRadius) ->
  Players = pewpew_arena_component:players(ArenaComponent),
  PlayersToCheck = [Player || Player <- Players, Player =/= ScanningPlayer],

  PlayersUnderRadar = players_under_radar(PlayersToCheck),

  #{
    players => PlayersUnderRadar,
    walls => []
  }.

players_under_radar([]) ->
  [];
players_under_radar(Players) ->
  ok.
