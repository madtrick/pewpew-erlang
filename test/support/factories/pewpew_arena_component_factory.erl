-module(pewpew_arena_component_factory).

-export([
  create/1
]).

create(Options) ->
  #{arena_options := ArenaOptions} = Options,
  {ok, ArenaComponent} = pewpew_arena_component:start_link(ArenaOptions),

  PlayersOptions = maps:get(players_options, Options, []),
  [
   pewpew_arena_component:create_player(ArenaComponent, PlayerOptions)
   || PlayerOptions <- PlayersOptions
  ],

  ArenaComponent.
