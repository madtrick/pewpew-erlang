-module(pewpew_command_context_data).

-export([
  new/2,
  context/1,
  command_data/1,
  pewpew_game/1,
  origin/1,
  players_origins/1
]).
-export([update/2]).

new(Context, CommandData) ->
  Options = [
    {context, Context},
    {command_data, CommandData},
    {pewpew_game, undefined},
    {origin, undefined}
  ],
  pewpew_map_backed_data:new(Options).

context(#{ context := Context }) -> Context.
command_data(#{ command_data := CommandData }) -> CommandData.
pewpew_game(#{ pewpew_game := PewpewGame }) -> PewpewGame.
origin(#{ origin := Origin }) -> Origin.
players_origins(CommandContextData) ->
  PewPewGame     = pewpew_game(CommandContextData),
  ArenaComponent = pewpew_game:arena_component(PewPewGame),
  Players        = pewpew_arena_component:players(ArenaComponent),

  lists:map(fun(Player) ->
    pewpew_player_component:channel(Player)
  end, Players).

update(PewPewCommandContextData, Options) ->
  pewpew_map_backed_data:update(PewPewCommandContextData, Options).
