-module(avioneta_games_sup).
-behaviour(supervisor).


-export([start_link/0, add_game/1]).
-export([init/1]).

-define(CHILD(I, Type, Options), {I, {I, start_link, Options}, temporary, 5000, Type, [I]}).


start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

add_game(GameName) ->
  supervisor:start_child(?MODULE, [GameName]).

init([]) ->
  {ok, { {simple_one_for_one, 5, 10}, [?CHILD(avioneta_game_sup, supervisor, [])]} }.


