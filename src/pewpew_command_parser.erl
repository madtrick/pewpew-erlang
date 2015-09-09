-module(pewpew_command_parser).
-include_lib("eunit/include/eunit.hrl").

-export([parse/1]).

-define(COMMAND(CommandType, Data), {[{<<"type">>, <<CommandType>>}, {<<"data">>, Data}]}).

parse(<<"">>) ->
  [];
parse([]) ->
  [];
parse(Data) ->
  JSON = jiffy:decode(Data),
  extract_command_from_json(JSON).

extract_command_from_json(JSON) ->
  build_command(JSON).


build_command(?COMMAND("ShotHitPlayerCommand", Data)) ->
  pewpew_command_context_data:new(pewpew_shot_hit_player_command_context, pewpew_shot_hit_player_command:fromJSON(Data));
build_command(?COMMAND("RegisterPlayerCommand", Data)) ->
  pewpew_command_context_data:new(pewpew_register_player_context, register_player_command:fromJSON(Data));
build_command(?COMMAND("MovePlayerCommand", Data)) ->
  pewpew_command_context_data:new(pewpew_move_player_context, pewpew_move_player_command:fromJSON(Data));
build_command(?COMMAND("PlayerShootCommand", Data)) ->
  pewpew_command_context_data:new(pewpew_player_shoot_context, pewpew_player_shoot_command:fromJSON(Data));
build_command(?COMMAND("DestroyPlayerCommand", Data)) ->
  pewpew_command_context_data:new(pewpew_destroy_player_context, pewpew_destroy_player_command:fromJSON(Data));
build_command(?COMMAND("RotatePlayerCommand", Data)) ->
  pewpew_command_context_data:new(pewpew_rotate_player_context, pewpew_rotate_player_command:fromJSON(Data));
build_command(?COMMAND("StartGameCommand", Data)) ->
  pewpew_command_context_data:new(pewpew_start_game_context, pewpew_start_game_command:fromJSON(Data));
build_command(?COMMAND("ConfigurePlayerCommand", Data)) ->
  pewpew_command_context_data:new(pewpew_configure_player_context, pewpew_configure_player_command:fromJSON(Data)).
