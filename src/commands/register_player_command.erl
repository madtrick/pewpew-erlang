-module(register_player_command).

-export([fromJSON/1, run/2]).

% {player : {}}
fromJSON(JSON) ->
  {[]} = JSON,
  pewpew_register_player_command_data:new(?MODULE, []).

run(_CommandData, ContextData) ->
  lager:debug("Running register_player_command"),
  ArenaComponent = arena_component(ContextData),
  register_player_if(can_register_player(ArenaComponent), ArenaComponent, [{origin, origin(ContextData)}]).

register_player_if(true, ArenaComponent, Data) ->
  {registered, pewpew_arena_component:create_player(ArenaComponent, Data)};
register_player_if(false, _, _) ->
  {not_registered, arena_full}.


can_register_player(ArenaComponent) ->
  pewpew_arena_component:positions_left(ArenaComponent) > 0.

arena_component(ContextData) ->
  pewpew_game:arena_component(pewpew_game(ContextData)).

pewpew_game(ContextData) ->
  pewpew_command_context_data:pewpew_game(ContextData).

origin(ContextData) ->
  pewpew_command_context_data:origin(ContextData).
