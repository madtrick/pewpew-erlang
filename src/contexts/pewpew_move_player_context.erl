-module(pewpew_move_player_context).

-export([call/1]).

call(CommandContextData) ->
  CommandOriginChannel = pewpew_command_context_data:origin(CommandContextData),
  PewPewGame           = pewpew_command_context_data:pewpew_game(CommandContextData),
  ArenaComponent       = pewpew_game:arena_component(PewPewGame),

  IsStarted = pewpew_game:is_started(PewPewGame),
  Player    = pewpew_arena_component:get_player(ArenaComponent, CommandOriginChannel),

  case IsStarted of
    false ->
      InvalidCommandError = pewpew_invalid_command_error:new(CommandOriginChannel),
      {reply, [{send_to, CommandOriginChannel, InvalidCommandError}]};
    true ->
      case Player of
        undefined ->
          InvalidCommandError = pewpew_invalid_command_error:new(CommandOriginChannel),
          {reply, [{send_to, CommandOriginChannel, InvalidCommandError}]};
        _ ->
          ok
      end
  end.


  %CommandData = pewpew_command_context_data:command_data(UpdatedCommandContextData),

  %Player = (pewpew_command_data:runner(CommandData)):run(
  %  pewpew_command_data:runner_data(CommandData), UpdatedCommandContextData
  %),

  %MovePlayerOrder = pewpew_move_player_order:new(Player, [{direction, pewpew_move_player_command_data:direction(pewpew_command_data:runner_data(CommandData))}]),

  %{reply, [{send_to_others, [MovePlayerOrder]}]}.

