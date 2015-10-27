-module(pewpew_player_component_mod_tests).
-include_lib("eunit/include/eunit.hrl").

movement_test_() ->
  Cases = [
   #{
    movements => [{move, <<"forward">>}],
    expectation => {x, 1.0, y, 0.0}
   },
   #{
    movements => [{move, <<"backward">>}],
    expectation => {x, -1.0, y, 0.0}
   },
   #{
    movements => [{move, <<"forward">>}, {move, <<"backward">>}],
    expectation => {x, 0.0, y, 0.0}
   },
   #{
    movements => [{rotate, 30}, {move, <<"forward">>}, {rotate, 330}, {move, <<"backward">>}],
    expectation => {x, 0.0, y, 1.0}
   },
   #{
    movements => [{rotate, 30}, {move, <<"forward">>}, {move, <<"backward">>}],
    expectation => {x, 0.0, y, 0.0}
   },
   #{
    movements => [{rotate, 30}, {move, <<"forward">>}],
    expectation => {x, 0.86603, y, 0.5}
   },
   #{
    movements => [{move, <<"backward">>}, {move, <<"forward">>}],
    expectation => {x, 0.0, y, 0.0}
   },
   #{
    speed => 2,
    movements => [{move, <<"forward">>}],
    expectation => {x, 2.0, y, 0.0}
   },
   #{
    speed => 2,
    movements => [{rotate, 90}, {move, <<"forward">>}],
    expectation => {x, 0.0, y, 2.0}
   }
  ],

  lists:map(fun(Case) ->
        #{movements := Movements, expectation := Expectation} = Case,
        Speed = maps:get(speed, Case, 1),
        InitialCoordinates =  maps:get(origin, Case, {x, 0, y, 0}),
        InitialRotation = maps:get(rotation, Case, 0),

        movement_sequence_test(Speed, InitialCoordinates, InitialRotation, Movements, Expectation)
  end, Cases).

movement_sequence_test(Speed, InitialCoordinates, InitialRotation, Movements, ExpectedCoordinates) ->
  {x, InitialX, y, InitialY} = InitialCoordinates,
  PlayerOptions = [
    {x, InitialX},
    {y, InitialY},
    {rotation, InitialRotation},
    {speed, Speed}
  ],

  PlayerComponentData = pewpew_player_component_data:new(PlayerOptions),

  UpdatedComponendData = lists:foldl(
      fun({move, Direction}, Data) ->
          {ok, NewData} = pewpew_player_component_mod:move(Direction, Data),
          NewData;
         ({rotate, Rotation}, Data) ->
          pewpew_player_component_data:update(Data, [{rotation, Rotation}])
      end
      ,PlayerComponentData, Movements),

  NewX = pewpew_player_component_data:x(UpdatedComponendData),
  NewY = pewpew_player_component_data:y(UpdatedComponendData),
  {x, ExpectedX, y, ExpectedY} = ExpectedCoordinates,

  [
    ?_assertEqual(ExpectedX, NewX),
    ?_assertEqual(ExpectedY, NewY)
  ].
