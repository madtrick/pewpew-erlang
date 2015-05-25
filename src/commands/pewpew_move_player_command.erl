-module(pewpew_move_player_command).

-export([
  fromJSON/1,
  run/2,
  is_valid/1
]).

% {player : , direction: }
fromJSON(JSON) ->
  {[{<<"player">>, Id}, {<<"direction">>, Direction}]} = JSON,
  pewpew_move_player_command_data:new(?MODULE, [{id, Id}, {direction, Direction}]).

is_valid(CommandData) ->
  Direction = pewpew_move_player_command_data:direction(CommandData),

  case Direction of
    <<"forward">> -> true;
    <<"backward">> -> true;
    _ -> false
  end.

run(CommandData, ContextData) ->
  lager:debug("Running move_player_command"),
  Channel = pewpew_command_context_data:origin(ContextData),
  Player  = pewpew_arena_component:get_player(arena_component(ContextData), Channel),
  pewpew_player_component:move(Player, [{direction, direction(CommandData)}]),
  Player.

arena_component(ContextData) ->
  pewpew_game:arena_component(
    pewpew_command_context_data:pewpew_game(ContextData)
  ).

direction(CommandData) ->
  pewpew_move_player_command_data:direction(CommandData).
