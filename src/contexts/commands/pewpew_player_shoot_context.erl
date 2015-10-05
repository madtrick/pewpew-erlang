-module(pewpew_player_shoot_context).
-include_lib("eunit/include/eunit.hrl").

-export([call/1]).

call(CommandContextData) ->
  CommandOriginChannel = pewpew_command_context_data:origin(CommandContextData),

  CommandData            = pewpew_command_context_data:command_data(CommandContextData),
  CommandModule          = pewpew_command_data:command_module(CommandData),
  PlayerShootCommandData = pewpew_command_data:command_data(CommandData),
  IsValidCommand         = CommandModule:is_valid(PlayerShootCommandData),
  PewPewGame             = pewpew_command_context_data:pewpew_game(CommandContextData),
  ArenaComponent         = pewpew_game:arena_component(PewPewGame),
  IsGameStarted          = pewpew_game:is_started(PewPewGame),

  case IsValidCommand of
    false ->
      InvalidCommandError = pewpew_invalid_command_error:new(CommandOriginChannel),
      {reply, [{send_to, CommandOriginChannel, InvalidCommandError}]};
    true ->
      case IsGameStarted of
        false ->
          InvalidCommandError = pewpew_invalid_command_error:new(CommandOriginChannel),
          {reply, [{send_to, CommandOriginChannel, InvalidCommandError}]};
        true ->
          CommandModule:run(PlayerShootCommandData, CommandContextData),
          Player       = pewpew_arena_component:get_player(ArenaComponent, CommandOriginChannel),
          ShootingInfo = pewpew_player_component:shooting_info(Player),

          PlayerShootAck = pewpew_player_shoot_ack:new(CommandOriginChannel, ShootingInfo),
          {reply, [{send_to, CommandOriginChannel, PlayerShootAck}]}
      end
  end.
