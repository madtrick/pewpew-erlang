-module(pewpew_arena_component_mod_tests).
-include_lib("eunit/include/eunit.hrl").

get_player_using_channel_test_() ->
  OriginFun          = fun() -> Fun = fun(F)-> timer:sleep(1000), F(F) end, Fun(Fun) end,
  Origin             = spawn(OriginFun),
  PlayerOptions      = [ {id, player_1}, {origin, Origin} ],
  GameContextData    = fake_game_context_data,
  {ok, Player}       = pewpew_player_component:start_link(GameContextData, PlayerOptions),
  ArenaComponentData = pewpew_arena_component_data:new([{players, [Player]}]),
  {ok, Result}       = pewpew_arena_component_mod:get_player(Origin, ArenaComponentData),

  [?_assertEqual(Player, Result)].

get_player_using_id_test_() ->
  OriginFun          = fun() -> Fun = fun(F)-> timer:sleep(1000), F(F) end, Fun(Fun) end,
  Origin             = spawn(OriginFun),
  PlayerOptions      = [ {id, player_1}, {origin, Origin} ],
  GameContextData    = fake_game_context_data,
  {ok, Player}       = pewpew_player_component:start_link(GameContextData, PlayerOptions),
  ArenaComponentData = pewpew_arena_component_data:new([{players, [Player]}]),
  {ok, Result}       = pewpew_arena_component_mod:get_player(player_1, ArenaComponentData),

  [?_assertEqual(Player, Result)].
