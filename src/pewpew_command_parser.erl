-module(pewpew_command_parser).

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
build_command(?COMMAND("ShootPlayerCommand", Data)) ->
  pewpew_command_context_data:new(pewpew_shoot_player_context, pewpew_shoot_player_command:fromJSON(Data));
build_command(?COMMAND("DestroyPlayerCommand", Data)) ->
  pewpew_command_context_data:new(pewpew_destroy_player_context, pewpew_destroy_player_command:fromJSON(Data));
build_command(?COMMAND("RotatePlayerCommand", Data)) ->
  pewpew_command_context_data:new(pewpew_rotate_player_context, pewpew_rotate_player_command:fromJSON(Data)).
