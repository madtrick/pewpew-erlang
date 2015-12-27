-module(pewpew_arena_component_mod_tests).
-include_lib("eunit/include/eunit.hrl").

get_player_using_channel_test_() ->
  OriginFun          = fun() -> Fun = fun(F)-> timer:sleep(1000), F(F) end, Fun(Fun) end,
  Origin             = spawn(OriginFun),
  PlayerOptions      = [ {id, player_1}, {origin, Origin} ],
  {ok, Player}       = pewpew_player_component:start_link(PlayerOptions),
  ArenaComponentData = pewpew_arena_component_data:new([{players, [Player]}]),
  {ok, Result}       = pewpew_arena_component_mod:get_player(Origin, ArenaComponentData),

  [?_assertEqual(Player, Result)].

get_player_using_id_test_() ->
  OriginFun          = fun() -> Fun = fun(F)-> timer:sleep(1000), F(F) end, Fun(Fun) end,
  Origin             = spawn(OriginFun),
  PlayerOptions      = [ {id, player_1}, {origin, Origin} ],
  {ok, Player}       = pewpew_player_component:start_link(PlayerOptions),
  ArenaComponentData = pewpew_arena_component_data:new([{players, [Player]}]),
  {ok, Result}       = pewpew_arena_component_mod:get_player(player_1, ArenaComponentData),

  [?_assertEqual(Player, Result)].

when_multiple_shots_are_destroyed_in_one_cycle_all_are_removed_from_the_arena_test() ->
  % Presuppose that the shots have been already moved
  {ok, Shot1} = pewpew_shot_component:start_link([{rotation, 0}, {x, 200}, {y, 200}, {id, 1}, {speed, 1.5}]),
  {ok, Shot2} = pewpew_shot_component:start_link([{rotation, 0}, {x, 200.5}, {y, 100}, {id, 2}, {speed, 1.5}]),
  {ok, Shot3} = pewpew_shot_component:start_link([{rotation, 0}, {x, 10}, {y, 100}, {id, 3}, {speed, 1.5}]),

  Shots = [Shot1, Shot2, Shot3],
  ArenaComponentData = pewpew_arena_component_data:new([{shots, Shots}, {width, 200}, {height, 400}]),
  {_, RemainingShots} = pewpew_arena_component_mod:update_shots(ArenaComponentData),

  ?assertEqual([Shot3], RemainingShots).
