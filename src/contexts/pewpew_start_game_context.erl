-module(pewpew_start_game_context).

-export([call/1]).

call(CommandContextData) ->
  CommandOriginChannel = pewpew_command_context_data:origin(CommandContextData),
  ChannelConfig = pewpew_channel:config(CommandOriginChannel),

  IsControl = proplists:get_value(is_control, ChannelConfig),

  case IsControl of
    true ->
      % start game
      StartGameAck = pewpew_start_game_ack:new(CommandOriginChannel),
      {reply, [{send_to_origin, StartGameAck}]};
      % return ack
      % send start order to players
    false ->
      % send error
      nope
  end.
