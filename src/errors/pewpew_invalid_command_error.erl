-module(pewpew_invalid_command_error).

-export([new/1, toJSON/1]).

new(Channel) ->
  Values = [
     {channel, Channel}
  ],
  pewpew_invalid_command_error_data:new(?MODULE, Values).

toJSON(ErrorData) ->
  pewpew_invalid_command_error_serializer:toJSON(ErrorData).
