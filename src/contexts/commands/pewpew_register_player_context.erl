-module(pewpew_register_player_context).
-include_lib("eunit/include/eunit.hrl").

-export([call/1]).

call(CommandContextData) ->
  CommandOriginChannel = pewpew_command_context_data:origin(CommandContextData),
  PewPewGame           = pewpew_command_context_data:pewpew_game(CommandContextData),
  ArenaComponent       = pewpew_game:arena_component(PewPewGame),
  Players              = pewpew_arena_component:players(ArenaComponent),

  EvalCommandGuard = not are_there_other_players_for_this_channel(Players, CommandOriginChannel),
  maybe_eval_command(EvalCommandGuard, CommandContextData, CommandOriginChannel).

maybe_eval_command(true, CommandContextData, CommandOriginChannel) ->
  CommandData = pewpew_command_context_data:command_data(CommandContextData),

  eval_result_from_command(
    (pewpew_command_data:command_module(CommandData)):run(
    pewpew_command_data:command_data(CommandData), CommandContextData
  ), CommandOriginChannel, CommandContextData);
maybe_eval_command(false, _, CommandOriginChannel) ->
  InvalidCommandError = pewpew_invalid_command_error:new(CommandOriginChannel),
  {reply, [{send_to, CommandOriginChannel, InvalidCommandError}]}.

eval_result_from_command({registered, NewPlayer}, CommandOriginChannel, CommandContextData) ->
  PewPewGame    = pewpew_command_context_data:pewpew_game(CommandContextData),
  IsGameStarted = pewpew_game:is_started(PewPewGame),

  RegisterPlayerAck = pewpew_register_player_ack:new(NewPlayer, CommandOriginChannel),

  InitialReplies = [{send_to, CommandOriginChannel, RegisterPlayerAck}],

  Replies = case IsGameStarted of
              false -> InitialReplies;
              true ->
                CanJoinStartedGame = pewpew_config:get(players_can_join_started_game, false),

                case CanJoinStartedGame of
                  true ->
                    StartGameOrder = pewpew_start_game_order:new(),
                    ExtraReply = {send_to, CommandOriginChannel, StartGameOrder},
                    lists:reverse([ExtraReply | InitialReplies]);
                  false ->
                    InitialReplies
                end
            end,

  {reply, Replies};
eval_result_from_command({not_registered, arena_full}, CommandOriginChannel, _) ->
  Message = pewpew_no_slots_left_notification:new(),
  {reply, [{send_to, CommandOriginChannel, Message}]}.

are_there_other_players_for_this_channel([], _) ->
  false;
are_there_other_players_for_this_channel([Player | Tail], Channel) ->
  case pewpew_player_component:channel(Player) of
    Channel -> true;
    _ -> are_there_other_players_for_this_channel(Tail, Channel)
  end.
