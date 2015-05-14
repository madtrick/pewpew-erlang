-module(pewpew_register_player_context).

-export([call/1]).

call(CommandContextData) ->
  CommandOriginChannel = pewpew_command_context_data:origin(CommandContextData),
  Game                 = pewpew_command_context_data:pewpew_game(CommandContextData),
  ArenaComponent       = pewpew_game:arena_component(Game),
  Players              = pewpew_arena_component:players(ArenaComponent),

  EvalCommandGuard = not are_there_other_players_for_this_channel(Players, CommandOriginChannel),
  maybe_eval_command(EvalCommandGuard, CommandContextData, CommandOriginChannel).

maybe_eval_command(true, CommandContextData, CommandOriginChannel) ->
  CommandData = pewpew_command_context_data:command_data(CommandContextData),

  eval_result_from_command(
    (pewpew_command_data:runner(CommandData)):run(
    pewpew_command_data:runner_data(CommandData), CommandContextData
  ), CommandOriginChannel);
maybe_eval_command(false, _, _) ->
  noreply.

eval_result_from_command({registered, NewPlayer}, CommandOriginChannel) ->
  RegisterPlayerAck = pewpew_register_player_ack:new(NewPlayer, CommandOriginChannel),
  {reply, [{send_to_origin, RegisterPlayerAck}]}.

are_there_other_players_for_this_channel([], _) ->
  false;
are_there_other_players_for_this_channel([Player | Tail], Channel) ->
  case pewpew_player_component:channel(Player) of
    Channel -> true;
    _ -> are_there_other_players_for_this_channel(Tail, Channel)
  end.
