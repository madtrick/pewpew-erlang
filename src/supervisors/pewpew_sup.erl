-module(pewpew_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(CHILD(I, Type, Options), {I, {I, start_link, Options}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, { {one_for_one, 5, 10}, [
        ?CHILD(wsserver_server, worker, [pewpew_wsserver:config()]),
        ?CHILD(pewpew_registry, worker, []),
        ?CHILD(pewpew_multicast, worker, []),
        ?CHILD(pewpew_games_sup, supervisor, []),
        ?CHILD(pewpew_core, worker, [])
      ]} }.

