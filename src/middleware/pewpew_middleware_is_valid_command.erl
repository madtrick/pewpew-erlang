-module(pewpew_middleware_is_valid_command).
-include_lib("eunit/include/eunit.hrl").

-export([run/2]).

run(CommandContextData, Next) ->
  CommandOriginChannel  = pewpew_command_context_data:origin(CommandContextData),
  CommandModule = pewpew_command_context_data:command_module(CommandContextData),
  CommandPayload = pewpew_command_context_data:command_payload(CommandContextData),

  case validate_command(CommandModule, CommandPayload) of
    true -> Next(CommandContextData);
    false ->
      InvalidCommandError = pewpew_invalid_command_error:new(CommandOriginChannel),
      {reply, [{send_to, CommandOriginChannel, InvalidCommandError}]}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
validate_command(CommandModule, CommandPayload) ->
  ExportsIsValid = erlang:function_exported(CommandModule, is_valid, 1),
  maybe_validate_command(CommandModule, CommandPayload, ExportsIsValid).

maybe_validate_command(_, _, false) ->
  true;
maybe_validate_command(CommandModule, CommandPayload, true) ->
  CommandModule:is_valid(CommandPayload).
