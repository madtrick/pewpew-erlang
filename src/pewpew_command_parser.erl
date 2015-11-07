-module(pewpew_command_parser).
-include_lib("eunit/include/eunit.hrl").

-export([parse/1]).

parse(<<"">>) ->
  [];
parse([]) ->
  [];
parse(Payload) ->
  {JSON} = jiffy:decode(Payload),
  Type = proplists:get_value(<<"type">>, JSON),
  Data = proplists:get_value(<<"data">>, JSON, {[]}),

  build_command(Type, Data).

build_command(<<"ShotHitPlayerCommand">>, Data) ->
  pewpew_command_context_data:new(pewpew_shot_hit_player_command_context, pewpew_shot_hit_player_command:fromJSON(Data));
build_command(<<"RegisterPlayerCommand">>, Data) ->
  pewpew_command_context_data:new(pewpew_register_player_context, register_player_command:fromJSON(Data));
build_command(<<"MovePlayerCommand">>, Data) ->
  pewpew_command_context_data:new(pewpew_move_player_context, pewpew_move_player_command:fromJSON(Data));
build_command(<<"PlayerShootCommand">>, Data) ->
  pewpew_command_context_data:new(pewpew_player_shoot_context, pewpew_player_shoot_command:fromJSON(Data));
build_command(<<"DestroyPlayerCommand">>, Data) ->
  pewpew_command_context_data:new(pewpew_destroy_player_context, pewpew_destroy_player_command:fromJSON(Data));
build_command(<<"StartGameCommand">>, Data) ->
  pewpew_command_context_data:new(pewpew_start_game_context, pewpew_start_game_command:fromJSON(Data));
build_command(<<"ConfigurePlayerCommand">>, Data) ->
  pewpew_command_context_data:new(pewpew_configure_player_context, pewpew_configure_player_command:fromJSON(Data)).
