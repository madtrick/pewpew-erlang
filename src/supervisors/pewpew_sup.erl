-module(pewpew_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(CHILD_ALT(Id, Module, Type, Options), {Id, {Module, start_link, Options}, permanent, 5000, Type, [Module]}).
-define(CHILD(I, Type, Options), {I, {I, start_link, Options}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, { {one_for_one, 5, 10}, [
        ?CHILD_ALT(wsserver_players_server, wsserver_server, worker, [pewpew_players_wsserver:config()]),
        ?CHILD_ALT(wsserver_control_server, wsserver_server, worker, [pewpew_control_wsserver:config()]),
        ?CHILD(pewpew_timer, worker, []),
        ?CHILD(pewpew_registry, worker, []),
        ?CHILD(pewpew_multicast, worker, []),
        ?CHILD(pewpew_games_sup, supervisor, []),
        ?CHILD(pewpew_core, worker, [])
      ]} }.
