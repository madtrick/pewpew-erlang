-module(pewpew_start_game_command).

-export([
  fromJSON/1,
  run/2
]).

% {}
fromJSON(JSON) ->
  {[]} =JSON,
  pewpew_start_game_command_data:new(?MODULE, []).

run(_CommandData, _CommandContextData) ->
  ok.
