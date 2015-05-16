-module(pewpew_start_game_context).

-export([call/1]).

call(CommandContextData) ->
  PewPewGame           = pewpew_command_context_data:pewpew_game(CommandContextData),
  CommandOriginChannel = pewpew_command_context_data:origin(CommandContextData),
  ChannelConfig        = pewpew_channel:config(CommandOriginChannel),

  IsControl = proplists:get_value(is_control, ChannelConfig),
  IsStarted = pewpew_game:is_started(PewPewGame),

  case IsControl of
    true ->
      case IsStarted of
        true ->
          InvalidCommandError = pewpew_invalid_command_error:new(CommandOriginChannel),
          {reply, [{send_to_origin, InvalidCommandError}]};
        false ->
          % start game
          pewpew_game:start_game(PewPewGame),
          StartGameAck = pewpew_start_game_ack:new(CommandOriginChannel),
          {reply, [{send_to_origin, StartGameAck}]}
          % send start order to players
      end;
    false ->
      % send error
      InvalidCommandError = pewpew_invalid_command_error:new(CommandOriginChannel),
      {reply, [{send_to_origin, InvalidCommandError}]}
  end.
