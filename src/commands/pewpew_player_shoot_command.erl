-module(pewpew_player_shoot_command).

-export([
  fromJSON/1,
  run/2,
  is_valid/1
]).

% {}
fromJSON(_JSON) ->
  pewpew_player_shoot_command_data:new(?MODULE, []).

is_valid(_) ->
  true.

run(_CommandData, _ContextData) ->
  lager:debug("Running player_shoot_command"),
  lol.
  %Channel = pewpew_command_context_data:origin(ContextData),
  %Arena = arena_component(ContextData),
  %Player = pewpew_arena_component:get_player(Arena, Channel),

  %pewpew_arena_component:create_shoot(Arena),

shoot_id() ->
  % NOTE: find a better way to encode this as a binary
  erlang:list_to_binary(["Shot-", erlang:integer_to_list(fserlangutils_time:microseconds_since_epoch())]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
arena_component(ContextData) ->
  pewpew_game:arena_component(
    pewpew_command_context_data:pewpew_game(ContextData)
  ).
