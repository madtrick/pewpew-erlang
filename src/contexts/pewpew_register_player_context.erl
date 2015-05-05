-module(pewpew_register_player_context).

-export([call/3]).

call(CommandContextData, PewpewGame, OriginChannel) ->
  UpdatedCommandContextData = pewpew_command_context_data:update(CommandContextData, [{origin, OriginChannel}, {pewpew_game, PewpewGame}]),
  CommandData = pewpew_command_context_data:command_data(UpdatedCommandContextData),

  eval_result_from_command(
    (pewpew_command_data:runner(CommandData)):run(
    pewpew_command_data:runner_data(CommandData), UpdatedCommandContextData
  ), PewpewGame).

eval_result_from_command({registered, NewPlayer}, PewpewGame) ->
  ArenaComponent = pewpew_game:arena_component(PewpewGame),

  RegisterNewLocalPlayerOrder = pewpew_register_player_order:new(NewPlayer, [{remote, false}]),
  RegisterNewRemotePlayerOrder = pewpew_register_player_order:new(NewPlayer, [{remote, true}]),
  RegisterPlayersOrder = [pewpew_register_player_order:new(Player, [{remote, true}]) || Player <- pewpew_arena_component:players(ArenaComponent), Player =/= NewPlayer],

  {reply, [{send_to_origin, [RegisterNewLocalPlayerOrder | RegisterPlayersOrder]}, {send_to_others, [RegisterNewRemotePlayerOrder]}]};
eval_result_from_command({not_registered, _Reason}, _PewpewGame) ->
  {close, [{send_to_origin, [pewpew_no_seats_left_notification:new()]}]}.
