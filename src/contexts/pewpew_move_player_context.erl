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
          PlayerState = pewpew_player_component:get_state(Player),
          CommandData = pewpew_command_context_data:command_data(CommandContextData),
          (pewpew_command_data:command_module(CommandData)):run(
            pewpew_command_data:command_data(CommandData), CommandContextData
          ),

          case validates(Player, ArenaComponent) of
            true ->
              ok;
            false ->
              pewpew_player_component:set_state(Player, PlayerState),
              InvalidCommandError = pewpew_invalid_command_error:new(CommandOriginChannel),
              {reply, [{send_to, CommandOriginChannel, InvalidCommandError}]}
          end
      end
  end.


validates(Player, ArenaComponent) ->
  Radius = pewpew_player_component:radius(Player),
  X      = pewpew_player_component:x(Player),
  Y      = pewpew_player_component:y(Player),

  {width, ArenaWidth, height, ArenaHeight} = pewpew_arena_component:dimensions(ArenaComponent),


  ((X + Radius) < ArenaWidth) andalso
  ((Y + Radius) < ArenaHeight) andalso
  ((X - Radius) > 0) andalso
  ((Y - Radius) > 0).
