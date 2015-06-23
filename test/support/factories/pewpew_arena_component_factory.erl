-module(pewpew_arena_component_factory).
-include_lib("eunit/include/eunit.hrl").

-export([
  create/1
]).

create(Options) ->
  #{arena_options := ArenaOptions} = Options,
  {ok, ArenaComponent} = pewpew_arena_component:start_link(ArenaOptions),

  PlayersOptions = maps:get(players_options, Options, []),
  create_players(ArenaComponent, PlayersOptions),

  ArenaComponent.

create_players(_, []) ->
  ok;
create_players(ArenaComponent, [PlayerOptions | Tail]) ->
  create_player(ArenaComponent, PlayerOptions),
  create_players(ArenaComponent, Tail).

create_player(ArenaComponent, Options) ->
  Origin = pewpew_channel_mock:start(),
  Radius = 5,
  X      = Radius,
  Y      = Radius,
  Defaults = [{x, X}, {y, Y}, {id, player_id}, {color, red}, {name, name}, {rotation, 0}, {origin, Origin}, {radius, Radius}],

  PlayerOptions = lists:foldl(
    fun(El, Acc) ->
        {OptionName, _} = El,

        PlayerOption = case proplists:get_value(OptionName, Options) of
          undefined -> El;
          Value -> {OptionName, Value}
        end,

        [PlayerOption | Acc]
    end,
    [],
    Defaults),

  pewpew_arena_component:create_player(ArenaComponent, PlayerOptions).
