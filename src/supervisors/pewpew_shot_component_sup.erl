-module(pewpew_shot_component_sup).
-behaviour(supervisor).


-export([start_link/0, add_shot/2]).
-export([init/1]).

-define(CHILD(I, Type, Options), {I, {I, start_link, Options}, temporary, 5000, Type, [I]}).


start_link() ->
  supervisor:start_link(?MODULE, []).

add_shot(Supervisor, Data) ->
  supervisor:start_child(Supervisor, [Data]).

init([]) ->
  {ok, { {simple_one_for_one, 5, 10}, [?CHILD(pewpew_shot_component, worker, [])]} }.
