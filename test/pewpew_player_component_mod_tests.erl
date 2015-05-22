-module(pewpew_player_component_mod_tests).
-include_lib("eunit/include/eunit.hrl").

move_player_test(OriginX, OriginY, Direction, ExpectedX, ExpectedY) ->
  PlayerOptions = [
                   {x, OriginX},
                   {y, OriginY},
                   {rotation, 0}
                  ],

  PlayerComponentData          = pewpew_player_component_data:new(PlayerOptions),
  {ok, NewPlayerComponentData} = pewpew_player_component_mod:move(Direction, PlayerComponentData),

  NewX = pewpew_player_component_data:x(NewPlayerComponentData),
  NewY = pewpew_player_component_data:y(NewPlayerComponentData),

  [
    ?_assertEqual(ExpectedX, NewX),
    ?_assertEqual(ExpectedY, NewY)
  ].

move_player_test_() ->
  [
   move_player_test(0, 0, <<"up">>, 1.0, 0.0),
   move_player_test(0, 0, <<"down">>, -1.0, 0.0)
  ].
