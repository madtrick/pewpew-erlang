-module(pewpew_player_component_mod_tests).
-include_lib("eunit/include/eunit.hrl").

move_up_player_test_() ->
  PlayerOptions = [
                   {color, red},
                   {id, player_id},
                   {x, 0},
                   {y, 0},
                   {radius, 5},
                   {origin, fake_origin},
                   {rotation, 0}
                  ],

  PlayerComponentData          = pewpew_player_component_data:new(PlayerOptions),
  {ok, NewPlayerComponentData} = pewpew_player_component_mod:move(<<"up">>, PlayerComponentData),

  NewX = pewpew_player_component_data:x(NewPlayerComponentData),
  NewY = pewpew_player_component_data:y(NewPlayerComponentData),

  [
    ?_assertEqual(1.0, NewX),
    ?_assertEqual(0.0, NewY)
  ].

move_down_player_test_() ->
  PlayerOptions = [
                   {color, red},
                   {id, player_id},
                   {x, 0},
                   {y, 0},
                   {radius, 5},
                   {origin, fake_origin},
                   {rotation, 0}
                  ],

  PlayerComponentData          = pewpew_player_component_data:new(PlayerOptions),
  {ok, NewPlayerComponentData} = pewpew_player_component_mod:move(<<"down">>, PlayerComponentData),

  NewX = pewpew_player_component_data:x(NewPlayerComponentData),
  NewY = pewpew_player_component_data:y(NewPlayerComponentData),

  [
    ?_assertEqual(-1.0, NewX),
    ?_assertEqual(0.0, NewY)
  ].
