-module(pewpew_configure_player_command).

-export([
  fromJSON/1,
  run/2,
  is_valid/1
]).

-define(VALID_OPS, [<<"radarType">>]).

% {op: , args: }
fromJSON(JSON) ->
  {JSONProperties} = JSON,
  Op               = proplists:get_value(<<"op">>, JSONProperties),
  Args             = proplists:get_value(<<"args">>, JSONProperties),

  pewpew_configure_player_command_data:new(?MODULE, [{op, Op}, {args, Args}]).

is_valid(CommandData) ->
  Op = pewpew_configure_player_command_data:op(CommandData),
  is_op_valid(Op).

run(CommandData, ContextData) ->
  lager:debug("Running configure player command"),
  Channel = pewpew_command_context_data:origin(ContextData),
  Player  = pewpew_arena_component:get_player(arena_component(ContextData), Channel),
  Op      = pewpew_configure_player_command_data:op(CommandData),
  Args    = pewpew_configure_player_command_data:args(CommandData),

  configure_player(Player, Op, Args).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_op_valid(Op) ->
  lists:any(fun(ValidOp) -> ValidOp =:= Op end, ?VALID_OPS).

configure_player(Player, Op, Args) ->
  pewpew_player_component:configure(Player, Op, Args).

arena_component(ContextData) ->
  pewpew_game:arena_component(
    pewpew_command_context_data:pewpew_game(ContextData)
  ).
