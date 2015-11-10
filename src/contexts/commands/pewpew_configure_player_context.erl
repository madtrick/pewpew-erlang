-module(pewpew_configure_player_context).
-include_lib("eunit/include/eunit.hrl").

-export([call/1]).

call(CommandContextData) ->
  CommandOriginChannel = pewpew_command_context_data:origin(CommandContextData),
  CommandPayload = pewpew_command_context_data:command_payload(CommandContextData),
  CommandModule = pewpew_command_context_data:command_module(CommandContextData),

  OkOrError = CommandModule:run(CommandPayload, CommandContextData),

  case OkOrError of
    ok ->
      ConfigurePlayerAck = pewpew_configure_player_ack:new(),
      {reply, [{send_to, CommandOriginChannel, ConfigurePlayerAck}]};
    error ->
      InvalidCommandError = pewpew_invalid_command_error:new(CommandOriginChannel),
      {reply, [{send_to, CommandOriginChannel, InvalidCommandError}]}
  end.
