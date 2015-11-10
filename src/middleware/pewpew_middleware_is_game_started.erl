-module(pewpew_middleware_is_game_started).

-export([run/2]).

run(CommandContextData, Next) ->
  CommandOriginChannel  = pewpew_command_context_data:origin(CommandContextData),
  PewPewGame = pewpew_command_context_data:pewpew_game(CommandContextData),

  case pewpew_game:is_started(PewPewGame) of
    true -> Next(CommandContextData);
    false ->
      InvalidCommandError = pewpew_invalid_command_error:new(CommandOriginChannel),
      {reply, [{send_to, CommandOriginChannel, InvalidCommandError}]}
  end.
