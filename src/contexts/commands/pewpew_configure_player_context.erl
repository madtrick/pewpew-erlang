-module(pewpew_configure_player_context).
-include_lib("eunit/include/eunit.hrl").

-export([call/1]).

call(CommandContextData) ->
  CommandOriginChannel = pewpew_command_context_data:origin(CommandContextData),
  PewPewGame           = pewpew_command_context_data:pewpew_game(CommandContextData),

  CommandData                = pewpew_command_context_data:command_data(CommandContextData),
  CommandModule              = pewpew_command_data:command_module(CommandData),
  ConfigurePlayerCommandData = pewpew_command_data:command_data(CommandData),
  IsGameStarted              = pewpew_game:is_started(PewPewGame),
  IsValidCommand             = CommandModule:is_valid(ConfigurePlayerCommandData),

  case IsValidCommand of
    true ->
      case IsGameStarted of
        false ->
          InvalidCommandError = pewpew_invalid_command_error:new(CommandOriginChannel),
          {reply, [{send_to, CommandOriginChannel, InvalidCommandError}]};
        true ->
          OkOrError = (pewpew_command_data:command_module(CommandData)):run(
            pewpew_command_data:command_data(CommandData), CommandContextData
          ),

          case OkOrError of
            ok ->
              ConfigurePlayerAck = pewpew_configure_player_ack:new(CommandOriginChannel),
              ?debugVal(ConfigurePlayerAck),
              {reply, [{send_to, CommandOriginChannel, ConfigurePlayerAck}]};
            error ->
              InvalidCommandError = pewpew_invalid_command_error:new(CommandOriginChannel),
              {reply, [{send_to, CommandOriginChannel, InvalidCommandError}]}
          end
      end;
    false ->
          InvalidCommandError = pewpew_invalid_command_error:new(CommandOriginChannel),
          {reply, [{send_to, CommandOriginChannel, InvalidCommandError}]}
  end.
