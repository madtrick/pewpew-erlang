-module(register_player_command).

-export([fromJSON/1, run/2]).

% {player : {}}
fromJSON(JSON) ->
  {[]} = JSON,
  avioneta_register_player_command_data:new(?MODULE, []).

run(_CommandData, ContextData) ->
  lager:debug("Running register_player_command"),
  ArenaComponent = arena_component(ContextData),
  register_player_if(can_register_player(ArenaComponent), ArenaComponent, [{origin, origin(ContextData)}]).

register_player_if(true, ArenaComponent, Data) ->
  {registered, avioneta_arena_component:create_player(ArenaComponent, Data)};
register_player_if(false, _, _) ->
  {not_registered, "Arena full"}.


can_register_player(ArenaComponent) ->
  avioneta_arena_component:positions_left(ArenaComponent) > 0.

arena_component(ContextData) ->
  avioneta_game:arena_component(avioneta_game(ContextData)).

avioneta_game(ContextData) ->
  avioneta_command_context_data:avioneta_game(ContextData).

origin(ContextData) ->
  avioneta_command_context_data:origin(ContextData).
