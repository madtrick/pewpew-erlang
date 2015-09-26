-module(pewpew_shooting_tests).
-include_lib("eunit/include/eunit.hrl").

-export([tests/0]).
-import(
  pewpew_test_support, [
    run_test/1,
    ws_client_send/2,
    ws_client_recv/1,
    ws_client_flush/1,
    ws_client_sel_recv/2,
    ws_client_sel_recv/2,
    register_player/0,
    register_player/1,
    get_player_for_client/2,
    validate_type_in_last_reply_test/2,
    validate_last_reply_data_for_type_test/3,
    place_player_at/2
]).

tests() ->
  %%TODO: test simultaneous hits to a player
  {"Shooting", [
          {"It rejects shot command when game has not been started",
            run_test(#{
              steps => [
                register_player(),
                ws_client_sel_recv(ws_player_client, <<"RegisterPlayerAck">>),
                ws_client_send(ws_player_client, #{type => <<"PlayerShootCommand">>, data => #{}}),
                ws_client_recv(ws_player_client),
                validate_type_in_last_reply_test(ws_player_client, <<"InvalidCommandError">>)
              ]
             })
          },
          {"It accepts shot player",
            run_test(#{
              steps => [
                register_player(),
                ws_client_sel_recv(ws_player_client, <<"RegisterPlayerAck">>),
                ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
                ws_client_sel_recv(ws_player_client, <<"StartGameOrder">>),
                ws_client_send(ws_player_client, #{type => <<"PlayerShootCommand">>, data => #{}}),
                ws_client_recv(ws_player_client),
                validate_type_in_last_reply_test(ws_player_client, <<"PlayerShootAck">>)
              ]
             })
          },
          {"It only creates one shot per command",
            run_test(#{
              steps => [
                register_player(),
                ws_client_sel_recv(ws_player_client, <<"RegisterPlayerAck">>),
                place_player_at(ws_player_client, [{x, 200}, {y, 200}]),
                ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
                ws_client_sel_recv(ws_player_client, <<"StartGameOrder">>),
                ws_client_send(ws_player_client, #{type => <<"PlayerShootCommand">>, data => #{}}),
                ws_client_sel_recv(ws_player_client, <<"PlayerShootAck">>),
                ws_client_send(ws_player_client, #{type => <<"PlayerShootCommand">>, data => #{}}),
                ws_client_sel_recv(ws_player_client, <<"PlayerShootAck">>)
              ],

              test => fun (Context) ->
                          #{pewpew_game := PewPewGame} = Context,
                          ArenaComponent = pewpew_game:arena_component(PewPewGame),
                          Shots = pewpew_arena_component:shots(ArenaComponent),
                          NumberOfShots = erlang:length(Shots),

                          ?_assertEqual(2, NumberOfShots)
              end
            })
          },
          {"Shot is created at player boundary with player rotation",
            run_test(#{
              steps => [
                register_player(),
                ws_client_sel_recv(ws_player_client, <<"RegisterPlayerAck">>),
                ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
                ws_client_sel_recv(ws_player_client, <<"StartGameOrder">>),
                ws_client_send(ws_player_client, #{type => <<"PlayerShootCommand">>, data => #{}}),
                ws_client_sel_recv(ws_player_client, <<"PlayerShootAck">>)
              ],

              test => fun(Context) ->
                #{pewpew_game := PewPewGame} = Context,
                ArenaComponent = pewpew_game:arena_component(PewPewGame),
                [Shot] = pewpew_arena_component:shots(ArenaComponent),
                Player = get_player_for_client(ws_player_client, Context),

                PlayerRotation           = pewpew_player_component:rotation(Player),
                PlayerRadius             = pewpew_player_component:radius(Player),
                {x, PlayerX, y, PlayerY} = pewpew_player_component:coordinates(Player),
                {x, ShotX, y, ShotY}     = pewpew_shot_component:coordinates(Shot),
                ShotRotation             = pewpew_shot_component:rotation(Shot),

                ExpectedX = (PlayerX + PlayerRadius) * 1.0,
                ExpectedY = PlayerY * 1.0,

                [
                 ?_assertEqual(PlayerRotation, ShotRotation),
                 ?_assertEqual(ExpectedX, ShotX),
                 ?_assertEqual(ExpectedY, ShotY)
                ]
              end
             })
          },
          {"Shot is created at player boundary with player rotation (player rotated)",
            run_test(#{
              steps => [
                register_player(),
                ws_client_sel_recv(ws_player_client, <<"RegisterPlayerAck">>),
                ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
                ws_client_sel_recv(ws_player_client, <<"StartGameOrder">>),
                ws_client_send(ws_player_client, #{type => <<"MovePlayerCommand">>, data => [#{rotate => 35}]}),
                ws_client_sel_recv(ws_player_client, <<"MovePlayerAck">>),
                ws_client_send(ws_player_client, #{type => <<"PlayerShootCommand">>, data => #{}}),
                ws_client_sel_recv(ws_player_client, <<"PlayerShootAck">>)
              ],

              test => fun(Context) ->
                #{pewpew_game := PewPewGame} = Context,
                ArenaComponent = pewpew_game:arena_component(PewPewGame),
                [Shot] = pewpew_arena_component:shots(ArenaComponent),
                Player = get_player_for_client(ws_player_client, Context),

                PlayerRotation           = pewpew_player_component:rotation(Player),
                PlayerRadius             = pewpew_player_component:radius(Player),
                PlayerRotationInRadians  = PlayerRotation * math:pi() / 180,
                {x, PlayerX, y, PlayerY} = pewpew_player_component:coordinates(Player),
                {x, ShotX, y, ShotY}     = pewpew_shot_component:coordinates(Shot),
                ShotRotation             = pewpew_shot_component:rotation(Shot),

                X = math:cos(PlayerRotationInRadians) * (PlayerRadius),
                Y = math:sin(PlayerRotationInRadians) * (PlayerRadius),
                ExpectedX = X + PlayerX,
                ExpectedY = Y + PlayerY,

                [
                 ?_assertEqual(PlayerRotation, ShotRotation),
                 ?_assertEqual(ExpectedX, ShotX),
                 ?_assertEqual(ExpectedY, ShotY)
                ]
              end
             })
          },
          {"Shots moves forward with its rotation",
            run_test(#{
              steps => [
                register_player(),
                ws_client_sel_recv(ws_player_client, <<"RegisterPlayerAck">>),
                ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
                ws_client_sel_recv(ws_player_client, <<"StartGameOrder">>),
                ws_client_send(ws_player_client, #{type => <<"PlayerShootCommand">>, data => #{}}),
                ws_client_sel_recv(ws_player_client, <<"PlayerShootAck">>),
                fun (Context) ->
                  #{pewpew_game := PewPewGame} = Context,
                  ArenaComponent  = pewpew_game:arena_component(PewPewGame),
                  [Shot]          = pewpew_arena_component:shots(ArenaComponent),
                  ShotCoordinates = pewpew_shot_component:coordinates(Shot),

                  UpdatedContext = Context#{initial_shot_coordinates => ShotCoordinates},

                  {context, UpdatedContext}
                end,
                ws_client_flush(ws_control_client),
                ws_client_sel_recv(ws_control_client, <<"GameSnapshotNotification">>)
              ],

              test => fun(Context) ->
                #{
                  pewpew_game := PewPewGame,
                  initial_shot_coordinates := InitialShotCoordinates
                } = Context,

                {x, InitialShotX, y, InitialShotY} = InitialShotCoordinates,
                ArenaComponent       = pewpew_game:arena_component(PewPewGame),
                [Shot]               = pewpew_arena_component:shots(ArenaComponent),
                ShotRotation         = pewpew_shot_component:rotation(Shot),
                Radians              = math:pi() * ShotRotation / 180,
                {x, ShotX, y, ShotY} = pewpew_shot_component:coordinates(Shot),

                DX        = 1.5 * math:cos(Radians),
                DY        = 1.5 * math:sin(Radians),
                ExpectedX = InitialShotX + DX,
                ExpectedY = InitialShotY + DY,

                [
                 ?_assertEqual(ExpectedX, ShotX),
                 ?_assertEqual(ExpectedY, ShotY)
                ]
              end
             })
          },
          {"Shots is destroyed when it hits the wall",
            run_test(#{
              steps => [
                register_player(),
                ws_client_sel_recv(ws_player_client, <<"RegisterPlayerAck">>),
                place_player_at(ws_player_client, [{x, 799}, {y, 300}]),
                ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
                ws_client_sel_recv(ws_player_client, <<"StartGameOrder">>),
                ws_client_send(ws_player_client, #{type => <<"PlayerShootCommand">>, data => #{}}),
                ws_client_sel_recv(ws_player_client, <<"PlayerShootAck">>),
                ws_client_flush(ws_control_client),
                ws_client_sel_recv(ws_control_client, <<"GameSnapshotNotification">>),
                ws_client_sel_recv(ws_control_client, <<"GameSnapshotNotification">>)
              ],

              test => fun(Context) ->
                #{ pewpew_game := PewPewGame } = Context,

                ArenaComponent = pewpew_game:arena_component(PewPewGame),
                Shots          = pewpew_arena_component:shots(ArenaComponent),

                [
                 ?_assertEqual([], Shots)
                ]
              end
             })
          },
          {"Shot reduce player life when it hits the player",
            run_test(#{
              steps => [
                register_player(ws_player_1_client),
                register_player(ws_player_2_client),
                ws_client_sel_recv(ws_player_1_client, <<"RegisterPlayerAck">>),
                ws_client_sel_recv(ws_player_2_client, <<"RegisterPlayerAck">>),
                place_player_at_others_boundary(),
                ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
                ws_client_sel_recv(ws_player_1_client, <<"StartGameOrder">>),
                ws_client_send(ws_player_1_client, #{type => <<"PlayerShootCommand">>, data => #{}}),
                ws_client_sel_recv(ws_player_1_client, <<"PlayerShootAck">>),
                ws_client_flush(ws_control_client),
                ws_client_sel_recv(ws_control_client, <<"GameSnapshotNotification">>)
              ],

              test => fun (Context) ->
                Player2 = get_player_for_client(ws_player_2_client, Context),
                Player2Life = pewpew_player_component:life(Player2),

                ?_assertEqual(95, Player2Life)
              end
             })
          },
          {"Shot it destroyed when it hits a player",
            run_test(#{
              steps => [
                register_player(ws_player_1_client),
                register_player(ws_player_2_client),
                ws_client_sel_recv(ws_player_1_client, <<"RegisterPlayerAck">>),
                ws_client_sel_recv(ws_player_2_client, <<"RegisterPlayerAck">>),
                place_player_at_others_boundary(),
                ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
                ws_client_sel_recv(ws_player_1_client, <<"StartGameOrder">>),
                ws_client_send(ws_player_1_client, #{type => <<"PlayerShootCommand">>, data => #{}}),
                ws_client_sel_recv(ws_player_1_client, <<"PlayerShootAck">>),
                ws_client_flush(ws_control_client),
                ws_client_sel_recv(ws_control_client, <<"GameSnapshotNotification">>)
              ],

              test => fun (Context) ->
                #{pewpew_game := PewPewGame} = Context,

                ArenaComponent = pewpew_game:arena_component(PewPewGame),
                Shots = pewpew_arena_component:shots(ArenaComponent),

                ?_assertEqual([], Shots)
              end
             })
          },
          {"Player receives notification when it is hit by a shot",
            run_test(#{
              steps => [
                register_player(ws_player_1_client),
                register_player(ws_player_2_client),
                ws_client_sel_recv(ws_player_1_client, <<"RegisterPlayerAck">>),
                ws_client_sel_recv(ws_player_2_client, <<"RegisterPlayerAck">>),
                place_player_at_others_boundary(),
                ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
                ws_client_sel_recv(ws_player_1_client, <<"StartGameOrder">>),
                ws_client_send(ws_player_1_client, #{type => <<"PlayerShootCommand">>, data => #{}}),
                ws_client_sel_recv(ws_player_1_client, <<"PlayerShootAck">>),
                ws_client_sel_recv(ws_player_2_client, <<"PlayerHitByShotNotification">>)
              ],

              test => fun (_) ->
                ExpectedData = #{<<"life">> => 95},

                validate_last_reply_data_for_type_test(ws_player_2_client, ExpectedData, <<"PlayerHitByShotNotification">>)
              end
             })
          },
          {"Player is destroyed when it is hit by enough shots",
            run_test(#{
              steps => [
                register_player(ws_player_1_client),
                register_player(ws_player_2_client),
                ws_client_sel_recv(ws_player_1_client, <<"RegisterPlayerAck">>),
                ws_client_sel_recv(ws_player_2_client, <<"RegisterPlayerAck">>),
                place_player_at_others_boundary(),
                ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
                ws_client_sel_recv(ws_player_1_client, <<"StartGameOrder">>),
                fun (_) ->
                  Steps = lists:seq(1, 20),
                  lists:map(fun(_) ->
                    fun(_) ->
                        [
                         ws_client_send(ws_player_1_client, #{type => <<"PlayerShootCommand">>, data => #{}}),
                         ws_client_sel_recv(ws_player_1_client, <<"PlayerShootAck">>)
                        ]
                    end
                  end, Steps)
                end,
                ws_client_sel_recv(ws_player_2_client, <<"PlayerDestroyedNotification">>)
              ],

              test => ?_assert(true)
             })
          },
          {"Destroyed player is removed from the arena",
            run_test(#{
              steps => [
                register_player(ws_player_1_client),
                register_player(ws_player_2_client),
                ws_client_sel_recv(ws_player_1_client, <<"RegisterPlayerAck">>),
                ws_client_sel_recv(ws_player_2_client, <<"RegisterPlayerAck">>),
                place_player_at_others_boundary(),
                ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
                ws_client_sel_recv(ws_player_1_client, <<"StartGameOrder">>),
                fun (_) ->
                  Steps = lists:seq(1, 20),
                  lists:map(fun(_) ->
                    fun(_) ->
                        [
                         ws_client_send(ws_player_1_client, #{type => <<"PlayerShootCommand">>, data => #{}}),
                         ws_client_sel_recv(ws_player_1_client, <<"PlayerShootAck">>)
                        ]
                    end
                  end, Steps)
                end,
                ws_client_sel_recv(ws_player_2_client, <<"PlayerDestroyedNotification">>)
              ],

              test => fun(Context) ->
                #{pewpew_game := PewPewGame} = Context,

                ArenaComponent = pewpew_game:arena_component(PewPewGame),
                Player1 = get_player_for_client(ws_player_1_client, Context),
                Players = pewpew_arena_component:players(ArenaComponent),

                ?_assertEqual([Player1], Players)
              end
             })
          }
        ]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
place_player_at_others_boundary() ->
  fun (Context) ->
      Player1 = get_player_for_client(ws_player_1_client, Context),
      Player2 = get_player_for_client(ws_player_2_client, Context),
      Radius  = pewpew_player_component:radius(Player1),

      pewpew_player_component:set_coordinates(Player1, [{x, 200}, {y, 200}]),
      pewpew_player_component:set_coordinates(Player2, [{x, 200 + 2 * Radius + 1}, {y, 200}]),

      ok
  end.
