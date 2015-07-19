-module(pewpew_configure_player_command).

-export([
  fromJSON/1,
  run/2,
  is_valid/1
]).

% {op: , args: }
fromJSON(JSON) ->
  {JSONProperties} = JSON,
  Op               = proplists:get_value(<<"op">>, JSONProperties),
  Args             = proplists:get_value(<<"args">>, JSONProperties),

  pewpew_configure_player_command_data:new(?MODULE, [{op, Op}, {args, Args}]).

is_valid(_) ->
  true.

run(_CommandData, _ContextData) ->
  lager:debug("Running configure player command"),
  lol.
