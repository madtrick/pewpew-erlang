-module(pewpew_configure_player_context).

-export([call/1]).

call(CommandContextData) ->
  CommandOriginChannel = pewpew_command_context_data:origin(CommandContextData),
  PewPewGame           = pewpew_command_context_data:pewpew_game(CommandContextData),

  IsGameStarted = pewpew_game:is_started(PewPewGame),

  case IsGameStarted of
    false ->
      InvalidCommandError = pewpew_invalid_command_error:new(CommandOriginChannel),
      {reply, [{send_to, CommandOriginChannel, InvalidCommandError}]}
  end.
