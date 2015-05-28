-module(pewpew_move_player_command).

-export([
  fromJSON/1,
  run/2,
  is_valid/1
]).

-define(ROTATE_KEY, <<"rotate">>).
-define(MOVE_KEY, <<"move">>).

% [{move: }, {rotate: }]
fromJSON(JSON) ->
  Movements = [ Object || {[Object]} <- JSON],
  pewpew_move_player_command_data:new(?MODULE, [{movements, Movements}]).

is_valid(CommandData) ->
  Movements = pewpew_move_player_command_data:movements(CommandData),
  are_movements_valid(Movements).

run(CommandData, ContextData) ->
  lager:debug("Running move_player_command"),
  Channel   = pewpew_command_context_data:origin(ContextData),
  Movements = pewpew_move_player_command_data:movements(CommandData),
  Player    = pewpew_arena_component:get_player(arena_component(ContextData), Channel),
  lists:foreach(fun(Movement) -> apply_movement(Movement, Player) end, Movements),
  Player.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
are_movements_valid([]) ->
  false;
are_movements_valid(Movements) ->
  are_movements_valid(Movements, 0, 0).

are_movements_valid([], _, _) ->
  true;
are_movements_valid(
  [{<<"rotate">>, Angle} | Tail],
  NumberOfRotations,
  NumberOfMoves
) when 0 =< Angle, Angle =< 360 ->

  NumberOfRotations =:= 0 andalso
  are_movements_valid(Tail, NumberOfRotations + 1, NumberOfMoves);
are_movements_valid(
  [{<<"move">>, <<"forward">>} | Tail],
  NumberOfRotations,
  NumberOfMoves
) ->
  NumberOfMoves =:= 0 andalso
  are_movements_valid(Tail, NumberOfRotations, NumberOfMoves + 1);
are_movements_valid(
  [{<<"move">>, <<"backward">>} | Tail],
  NumberOfRotations,
  NumberOfMoves
) ->
  NumberOfMoves =:= 0 andalso
  are_movements_valid(Tail, NumberOfRotations, NumberOfMoves + 1);
are_movements_valid(_, _, _) ->
  false.

apply_movement({?ROTATE_KEY, Degrees}, Player) ->
  pewpew_player_component:rotate(Player, Degrees);
apply_movement({?MOVE_KEY, Direction}, Player) ->
  pewpew_player_component:move(Player, [{direction, Direction}]).

arena_component(ContextData) ->
  pewpew_game:arena_component(
    pewpew_command_context_data:pewpew_game(ContextData)
  ).
