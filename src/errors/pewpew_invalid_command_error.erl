-module(pewpew_invalid_command_error).

-export([new/1]).

% TODO: remove the _Channel parameter
new(_Channel) ->
  pewpew_message:new(pewpew_invalid_command_error_serializer).
