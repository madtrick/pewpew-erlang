-module(pewpew_invalid_command_error_serializer).

-export([toJSON/1]).

toJSON(_ErrorData) ->
  Struct = {[
       {type, <<"InvalidCommandError">>},
       {data, {[]}}
    ]},
  Struct.
