-module(pewpew_radar).
-include_lib("eunit/include/eunit.hrl").

-export([scan/3]).

scan(ArenaComponent, ScanningPlayer, ScanRadius) ->
  Players = pewpew_arena_component:players(ArenaComponent),
  PlayersToCheck = [Player || Player <- Players, Player =/= ScanningPlayer],

  PlayersUnderRadar = players_under_radar(PlayersToCheck, ScanningPlayer, ScanRadius),

  #{
    players => PlayersUnderRadar,
    walls => []
  }.

players_under_radar([], _, _) ->
  [];
players_under_radar(Players, ScanningPlayer, ScanRadius) ->
  {x, ScanningPlayerX, y, ScanningPlayerY} = pewpew_player_component:coordinates(ScanningPlayer),

  lists:filter(
    fun(Player) ->
        {x, X, y, Y} = pewpew_player_component:coordinates(Player),

        A = erlang:abs(X - ScanningPlayerX),
        B = erlang:abs(Y - ScanningPlayerY),
        Distance = math:sqrt(A*A + B*B),

        Distance =< ScanRadius
    end,
   Players).
