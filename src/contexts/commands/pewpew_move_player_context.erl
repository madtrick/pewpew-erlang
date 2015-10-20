-module(pewpew_move_player_context).

-export([call/1]).

call(CommandContextData) ->
  CommandOriginChannel = pewpew_command_context_data:origin(CommandContextData),
  PewPewGame           = pewpew_command_context_data:pewpew_game(CommandContextData),
  ArenaComponent       = pewpew_game:arena_component(PewPewGame),

  CommandData           = pewpew_command_context_data:command_data(CommandContextData),
  CommandModule         = pewpew_command_data:command_module(CommandData),
  MovePlayerCommandData = pewpew_command_data:command_data(CommandData),
  IsStarted             = pewpew_game:is_started(PewPewGame),
  Player                = pewpew_arena_component:get_player(ArenaComponent, CommandOriginChannel),
  IsValidCommand        = CommandModule:is_valid(MovePlayerCommandData),

  case IsValidCommand of
    true ->
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
              (pewpew_command_data:command_module(CommandData)):run(
                pewpew_command_data:command_data(CommandData), CommandContextData
              ),

              case validates(Player, ArenaComponent) of
                true ->
                  Coordinates   = pewpew_player_component:coordinates(Player),
                  Rotation      = pewpew_player_component:rotation(Player),
                  MovePlayerAck = pewpew_move_player_ack:new(Coordinates, Rotation, CommandOriginChannel),
                  {reply, [{send_to, CommandOriginChannel, MovePlayerAck}]};
                false ->
                  pewpew_player_component:set_state(Player, PlayerState),
                  InvalidCommandError = pewpew_invalid_command_error:new(CommandOriginChannel),
                  {reply, [{send_to, CommandOriginChannel, InvalidCommandError}]}
              end
          end
      end;
    false ->
      InvalidCommandError = pewpew_invalid_command_error:new(CommandOriginChannel),
      {reply, [{send_to, CommandOriginChannel, InvalidCommandError}]}
  end.


% TODO move this into the command
validates(Player, ArenaComponent) ->
  not collides_with_walls(Player, ArenaComponent) andalso not collides_with_players(Player, ArenaComponent).

collides_with_players(Player, ArenaComponent) ->
  Radius = pewpew_player_component:radius(Player),
  X      = pewpew_player_component:x(Player),
  Y      = pewpew_player_component:y(Player),

  Players = pewpew_arena_component:players(ArenaComponent),

  lists:any(fun (P) ->
    case P =:= Player of
      true -> false;
      _ ->
        PR = pewpew_player_component:radius(P),
        PX = pewpew_player_component:x(P),
        PY = pewpew_player_component:y(P),

        % Formula got at http://stackoverflow.com/a/8367547/1078859
        % (R0-R1)^2 <= (x0-x1)^2+(y0-y1)^2 <= (R0+R1)^2
        Value = math:pow((PX - X), 2) + math:pow((PY - Y), 2),
        math:pow(Radius - PR, 2) =< Value andalso Value =< math:pow(Radius + PR, 2)
    end
  end, Players).

collides_with_walls(Player, ArenaComponent) ->
  Radius = pewpew_player_component:radius(Player),
  X      = pewpew_player_component:x(Player),
  Y      = pewpew_player_component:y(Player),

  {width, ArenaWidth, height, ArenaHeight} = pewpew_arena_component:dimensions(ArenaComponent),


  ((X + Radius) > ArenaWidth) orelse
  ((Y + Radius) > ArenaHeight) orelse
  ((X - Radius) < 0) orelse
  ((Y - Radius) < 0).
