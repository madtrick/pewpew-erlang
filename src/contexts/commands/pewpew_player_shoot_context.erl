-module(pewpew_player_shoot_context).
-include_lib("eunit/include/eunit.hrl").

-export([call/1]).

call(CommandContextData) ->
  CommandOriginChannel = pewpew_command_context_data:origin(CommandContextData),

  CommandPayload = pewpew_command_context_data:command_payload(CommandContextData),
  CommandModule  = pewpew_command_context_data:command_module(CommandContextData),
  PewPewGame     = pewpew_command_context_data:pewpew_game(CommandContextData),
  ArenaComponent = pewpew_game:arena_component(PewPewGame),

  Player       = pewpew_arena_component:get_player(ArenaComponent, CommandOriginChannel),
  InitialShootingInfo = pewpew_player_component:shooting_info(Player),
  #{cost := ShootingCost, tokens := ShootingTokens} = InitialShootingInfo,

  case ShootingTokens >= ShootingCost of
    true ->
      CommandModule:run(CommandPayload, CommandContextData),
      ShootingInfo = pewpew_player_component:shooting_info(Player),

      PlayerShootAck = pewpew_player_shoot_ack:new(ShootingInfo),
      {reply, [{send_to, CommandOriginChannel, PlayerShootAck}]};
    false ->
      InvalidCommandError = pewpew_invalid_command_error:new(CommandOriginChannel),
      {reply, [{send_to, CommandOriginChannel, InvalidCommandError}]}
  end.
