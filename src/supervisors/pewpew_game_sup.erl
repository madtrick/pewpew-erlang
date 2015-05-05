-module(pewpew_game_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(CHILD(I, Type, Options), {I, {I, start_link, Options}, temporary, 5000, Type, [I]}).

start_link(GameName) ->
  supervisor:start_link(?MODULE, [GameName]).

init([GameName]) ->
  {ok, { {one_for_one, 5, 10}, [
        ?CHILD(pewpew_game, worker, [GameName])
        %?CHILD(pewpew_arena_component, worker, [GameName]),
        %?CHILD(pewpew_player_component_sup, supervisor, [])
      ]} }.

