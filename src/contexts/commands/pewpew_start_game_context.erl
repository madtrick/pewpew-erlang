-module(pewpew_start_game_context).

-export([call/1, skip_middleware/0]).

call(CommandContextData) ->
  PewPewGame           = pewpew_command_context_data:pewpew_game(CommandContextData),
  CommandOriginChannel = pewpew_command_context_data:origin(CommandContextData),
  PlayersOrigins       = pewpew_command_context_data:players_origins(CommandContextData),
  ChannelConfig        = pewpew_channel:config(CommandOriginChannel),

  IsControl = proplists:get_value(is_control, ChannelConfig),
  IsStarted = pewpew_game:is_started(PewPewGame),

  case IsControl of
    true ->
      case IsStarted of
        true ->
          InvalidCommandError = pewpew_invalid_command_error:new(CommandOriginChannel),
          {reply, [{send_to, CommandOriginChannel, InvalidCommandError}]};
        false ->
          pewpew_game:start_game(PewPewGame),
          StartGameAck   = pewpew_start_game_ack:new(),
          StartGameOrder = pewpew_start_game_order:new(),
          {reply, [{send_to, CommandOriginChannel, StartGameAck}, {send_to, PlayersOrigins, StartGameOrder}]}
      end;
    false ->
      InvalidCommandError = pewpew_invalid_command_error:new(CommandOriginChannel),
      {reply, [{send_to, CommandOriginChannel, InvalidCommandError}]}
  end.

skip_middleware() ->
  [
    pewpew_middleware_is_game_started,
    pewpew_middleware_is_player_registered
  ].
