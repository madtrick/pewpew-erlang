-module(pewpew_register_player_context).

-export([call/1]).

call(CommandContextData) ->
  CommandData          = pewpew_command_context_data:command_data(CommandContextData),
  CommandOriginChannel = pewpew_command_context_data:origin(CommandContextData),

  eval_result_from_command(
    (pewpew_command_data:runner(CommandData)):run(
    pewpew_command_data:runner_data(CommandData), CommandContextData
  ), CommandOriginChannel).

eval_result_from_command({registered, NewPlayer}, CommandOriginChannel) ->
  % Create ACK
  % Reply with send to origin
  %ArenaComponent = pewpew_game:arena_component(PewpewGame),

  %RegisterNewLocalPlayerOrder = pewpew_register_player_order:new(NewPlayer, [{remote, false}]),
  %RegisterNewRemotePlayerOrder = pewpew_register_player_order:new(NewPlayer, [{remote, true}]),
  %RegisterPlayersOrder = [pewpew_register_player_order:new(Player, [{remote, true}]) || Player <- pewpew_arena_component:players(ArenaComponent), Player =/= NewPlayer],

  %{reply, [{send_to_origin, [RegisterNewLocalPlayerOrder | RegisterPlayersOrder]}, {send_to_others, [RegisterNewRemotePlayerOrder]}]};


  RegisterPlayerAck = pewpew_register_player_ack:new(NewPlayer, CommandOriginChannel),
  {reply, [{send_to_origin, RegisterPlayerAck}]}.
%eval_result_from_command({not_registered, _Reason}, CommandOriginChannel) ->
%  {close, [{send_to_origin, [pewpew_no_seats_left_notification:new()]}]}.
