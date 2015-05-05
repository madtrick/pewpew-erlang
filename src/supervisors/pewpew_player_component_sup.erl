-module(avioneta_player_component_sup).
-behaviour(supervisor).


-export([start_link/0, add_player/3]).
-export([init/1]).

-define(CHILD(I, Type, Options), {I, {I, start_link, Options}, temporary, 5000, Type, [I]}).


start_link() ->
  supervisor:start_link(?MODULE, []).

add_player(Supervisor, AvionetaGameContextData, Data) ->
  supervisor:start_child(Supervisor, [AvionetaGameContextData, Data]).

init([]) ->
  {ok, { {simple_one_for_one, 5, 10}, [?CHILD(avioneta_player_component, worker, [])]} }.

