-module(pewpew_middleware_is_player_registered).
-include_lib("eunit/include/eunit.hrl").

-export([run/2]).

run(CommandContextData, Next) ->
  CommandOriginChannel = pewpew_command_context_data:origin(CommandContextData),
  PewPewGame           = pewpew_command_context_data:pewpew_game(CommandContextData),
  ArenaComponent       = pewpew_game:arena_component(PewPewGame),

  Player               = pewpew_arena_component:get_player(ArenaComponent, CommandOriginChannel),

  case Player of
    undefined ->
      InvalidCommandError = pewpew_invalid_command_error:new(CommandOriginChannel),
      {reply, [{send_to, CommandOriginChannel, InvalidCommandError}]};
    _ ->
      Next(CommandContextData)
  end.
