-module(avioneta_register_player_context).

-export([call/3]).

call(CommandContextData, AvionetaGame, OriginChannel) ->
  UpdatedCommandContextData = avioneta_command_context_data:update(CommandContextData, [{origin, OriginChannel}, {avioneta_game, AvionetaGame}]),
  CommandData = avioneta_command_context_data:command_data(UpdatedCommandContextData),

  eval_result_from_command(
    (avioneta_command_data:runner(CommandData)):run(
    avioneta_command_data:runner_data(CommandData), UpdatedCommandContextData
  ), AvionetaGame).

eval_result_from_command({registered, NewPlayer}, AvionetaGame) ->
  ArenaComponent = avioneta_game:arena_component(AvionetaGame),

  RegisterNewLocalPlayerOrder = avioneta_register_player_order:new(NewPlayer, [{remote, false}]),
  RegisterNewRemotePlayerOrder = avioneta_register_player_order:new(NewPlayer, [{remote, true}]),
  RegisterPlayersOrder = [avioneta_register_player_order:new(Player, [{remote, true}]) || Player <- avioneta_arena_component:players(ArenaComponent), Player =/= NewPlayer],

  {reply, [{send_to_origin, [RegisterNewLocalPlayerOrder | RegisterPlayersOrder]}, {send_to_others, [RegisterNewRemotePlayerOrder]}]};
eval_result_from_command({not_registered, _Reason}, _AvionetaGame) ->
  {close, [{send_to_origin, [avioneta_no_seats_left_notification:new()]}]}.
