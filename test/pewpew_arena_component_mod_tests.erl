-module(pewpew_arena_component_mod_tests).
-include_lib("eunit/include/eunit.hrl").

move_player_test_() ->
  OriginFun = fun() -> Fun = fun(F)-> timer:sleep(1000), F(F) end, Fun(Fun) end,
  Origin = spawn(OriginFun),
  PlayerOptions = [
                   {color, red},
                   {id, player_id},
                   {x, 0},
                   {y, 0},
                   {radius, 5},
                   {origin, Origin}
                   % TODO: add a rotation
                  ],
  GameContextData = fake_game_context_data,
  {ok, Player} = pewpew_player_component:start_link(GameContextData, PlayerOptions),
  ArenaComponentData = pewpew_arena_component_data:new([{players, [Player]}]),

  Result = pewpew_arena_component_mod:move_player(player_id, {direction, <<"up">>}, ArenaComponentData),

  [?_assertEqual(ok, Result)].
