-module(pewpew_shooting_tests).
-include_lib("eunit/include/eunit.hrl").

-export([tests/0]).
-import(
  pewpew_test_support, [
    run_test/1,
    test_step/1,
    ws_client_send/2,
    ws_client_recv/1,
    ws_client_flush/1,
    ws_client_sel_recv/2,
    ws_client_sel_recv/2,
    register_player/0,
    register_player/1,
    get_last_reply_for_client/2,
    get_message_in_last_reply_for_client/3,
    get_player_for_client/2,
    validate_type_in_last_reply_test/2,
    validate_last_reply_data_for_type_test/3,
    validate_message_in_last_reply_matches/2,
    validate_message_matches/2,
    place_player_at/2
    ]).

tests() ->
  %%TODO: test simultaneous hits to a player
  {"Shooting", focus,[
      {"It rejects shot command when game has not been started",
       run_test(#{
            steps => [
              register_player(),
              ws_client_sel_recv(ws_player_client, <<"RegisterPlayerAck">>),
              ws_client_send(ws_player_client, #{type => <<"PlayerShootCommand">>, data => #{}}),
              ws_client_recv(ws_player_client),
              test_step(validate_type_in_last_reply_test(ws_player_client, <<"InvalidCommandError">>))
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
              test_step(validate_type_in_last_reply_test(ws_player_client, <<"PlayerShootAck">>))
              ]
            })
      },
      {"Shot ack includes information about the shooting tokens/cost",
       run_test(#{
            steps => [
              register_player(),
              ws_client_sel_recv(ws_player_client, <<"RegisterPlayerAck">>),
              fun (Context) ->
                  [LastReply] = get_last_reply_for_client(ws_player_client, Context),
                  ShootingCost = pewpew_utils:get_value_in_map([<<"data">>, <<"shooting">>, <<"cost">>], LastReply),
                  ShootingTokens = pewpew_utils:get_value_in_map([<<"data">>, <<"shooting">>, <<"tokens">>], LastReply),

                  UpdatedContext = maps:put(shooting_cost, ShootingCost, maps:put(shooting_tokens, ShootingTokens, Context)),
                  {context, UpdatedContext}
              end,
              ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
              ws_client_sel_recv(ws_player_client, <<"StartGameOrder">>),
              ws_client_send(ws_player_client, #{type => <<"PlayerShootCommand">>, data => #{}}),
              ws_client_sel_recv(ws_player_client, <<"PlayerShootAck">>)
              ],
            test => fun (Context) ->
                Message = get_message_in_last_reply_for_client(ws_player_client, <<"PlayerShootAck">>, Context),
                #{shooting_cost := ShootingCost, shooting_tokens := ShootingTokens } = Context,
                UpdatedShootingCost = pewpew_utils:get_value_in_map([<<"data">>, <<"shooting">>, <<"cost">>], Message),
                UpdatedShootingTokens = pewpew_utils:get_value_in_map([<<"data">>, <<"shooting">>, <<"tokens">>], Message),

                [
                  ?_assertEqual(UpdatedShootingCost, ShootingCost),
                  ?_assertEqual(UpdatedShootingTokens, ShootingTokens - ShootingCost)
                ]
            end
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
              fun (_) ->
                  % Increase intial tokens to be able to destroy the other player
                  % without having to worry about the rate limiting
                  pewpew_config:set([players, shooting, initial_tokens], 200),
                  pewpew_config:set([players, shooting, max_tokens], 200)
              end,
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
              fun (_) ->
                  % Increase intial tokens to be able to destroy the other player
                  % without having to worry about the rate limiting
                  pewpew_config:set([players, shooting, initial_tokens], 200),
                  pewpew_config:set([players, shooting, max_tokens], 200)
              end,
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
      },
      {"Destroyed player does not interfere with other shots",
       run_test(#{
            steps => [
              fun (_) ->
                  % Increase intial tokens to be able to destroy the other player
                  % without having to worry about the rate limiting
                  pewpew_config:set([players, shooting, initial_tokens], 200),
                  pewpew_config:set([players, shooting, max_tokens], 200)
              end,
              register_player(ws_player_1_client),
              register_player(ws_player_2_client),
              register_player(ws_player_3_client),
              ws_client_sel_recv(ws_player_1_client, <<"RegisterPlayerAck">>),
              ws_client_sel_recv(ws_player_2_client, <<"RegisterPlayerAck">>),
              ws_client_sel_recv(ws_player_3_client, <<"RegisterPlayerAck">>),
              fun (Context) ->
                  Player1 = get_player_for_client(ws_player_1_client, Context),
                  Player2 = get_player_for_client(ws_player_2_client, Context),
                  Player3 = get_player_for_client(ws_player_3_client, Context),
                  Radius  = pewpew_player_component:radius(Player1),

                  pewpew_player_component:set_coordinates(Player1, [{x, 200}, {y, 200}]),
                  pewpew_player_component:set_coordinates(Player2, [{x, 200 + 2 * Radius + 1}, {y, 200}]),
                  pewpew_player_component:set_coordinates(Player3, [{x, 200 + 4 * Radius + 1}, {y, 200}]),

                  ok
              end,
              ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
              ws_client_sel_recv(ws_player_1_client, <<"StartGameOrder">>),
              fun (_) ->
                  Steps = lists:seq(1, 30),
                  lists:map(fun(_) ->
                        fun(_) ->
                            [
                              ws_client_send(ws_player_1_client, #{type => <<"PlayerShootCommand">>, data => #{}}),
                              ws_client_sel_recv(ws_player_1_client, <<"PlayerShootAck">>)
                              ]
                        end
                    end, Steps)
              end
              ],

            test => fun(Context) ->
                Player3 = get_player_for_client(ws_player_3_client, Context),
                PlayersRadius = pewpew_player_component:radius(Player3),
                % 10 is the number of extra shots fired after destroying the player_2
                % 1.5 is the speed of the shoot
                ExpectedNumberOfHits = 10 - pewpew_utils:ceil((2 * PlayersRadius) / 1.5),
                Player3Life = pewpew_player_component:life(Player3),

                ?_assertEqual(100 - ExpectedNumberOfHits*5, Player3Life)
            end
            })
      },
      {"Game snapshots reflect reduction in players life",
       run_test(#{
            steps => [
              fun (_) ->
                  % Increase intial tokens to be able to destroy the other player
                  % without having to worry about the rate limiting
                  pewpew_config:set([players, shooting, initial_tokens], 200),
                  pewpew_config:set([players, shooting, max_tokens], 200)
              end,
              register_player(ws_player_1_client),
              register_player(ws_player_2_client),
              ws_client_sel_recv(ws_player_1_client, <<"RegisterPlayerAck">>),
              ws_client_sel_recv(ws_player_2_client, <<"RegisterPlayerAck">>),
              fun (Context) ->
                  Player1 = get_player_for_client(ws_player_1_client, Context),
                  Player2 = get_player_for_client(ws_player_2_client, Context),
                  Radius  = pewpew_player_component:radius(Player1),

                  pewpew_player_component:set_coordinates(Player1, [{x, 200}, {y, 200}]),
                  pewpew_player_component:set_coordinates(Player2, [{x, 200 + 2 * Radius + 1}, {y, 200}]),

                  ok
              end,
              ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
              ws_client_sel_recv(ws_control_client, <<"StartGameAck">>),
              fun (_) ->
                  Steps = lists:seq(1, 30),
                  lists:map(fun(_) ->
                        fun(_) ->
                            [
                              ws_client_send(ws_player_1_client, #{type => <<"PlayerShootCommand">>, data => #{}}),
                              ws_client_sel_recv(ws_player_1_client, <<"PlayerShootAck">>),
                              ws_client_sel_recv(ws_control_client, <<"GameSnapshotNotification">>),
                              fun (Context) ->
                                  #{pewpew_game := PewPewGame} = Context,
                                  ArenaComponent = pewpew_game:arena_component(PewPewGame),
                                  Players = pewpew_arena_component:players(ArenaComponent),
                                  ExpectedPlayerStates = lists:map(fun(Player) ->
                                          #{
                                            <<"id">> => pewpew_player_component:id(Player),
                                            <<"coordinates">> => #{
                                              <<"x">> => pewpew_player_component:x(Player),
                                              <<"y">> => pewpew_player_component:y(Player)
                                              },
                                            <<"life">> => pewpew_player_component:life(Player),
                                            <<"rotation">> => pewpew_player_component:rotation(Player),
                                            <<"radar">> => #{<<"type">> => <<"circular_scan">>, <<"radius">> => 40}
                                            }
                                      end, Players),

                                  ExpectedReply = #{
                                      <<"type">> => <<"GameSnapshotNotification">>,
                                      <<"data">> => #{
                                        <<"arena_snapshot">> => #{
                                          <<"players_snapshots">> => ExpectedPlayerStates,
                                          <<"shots_snapshots">> => '_'
                                          }
                                        }
                                      },

                                  test_step(validate_message_in_last_reply_matches(ws_control_client, ExpectedReply))
                              end
                              ]
                        end
                    end, Steps)
              end
              ]
            })
      },
      {"Destroyed player is not included in game snapshot notifications",
       run_test(#{
            steps => [
              fun (_) ->
                  pewpew_config:set([players, shooting, initial_tokens], 200),
                  pewpew_config:set([players, shooting, max_tokens], 200)
              end,
              register_player(ws_player_1_client),
              register_player(ws_player_2_client),
              ws_client_sel_recv(ws_player_1_client, <<"RegisterPlayerAck">>),
              ws_client_sel_recv(ws_player_2_client, <<"RegisterPlayerAck">>),
              fun (Context) ->
                  Player1 = get_player_for_client(ws_player_1_client, Context),
                  Player2 = get_player_for_client(ws_player_2_client, Context),
                  Radius  = pewpew_player_component:radius(Player1),

                  pewpew_player_component:set_coordinates(Player1, [{x, 200}, {y, 200}]),
                  pewpew_player_component:set_coordinates(Player2, [{x, 200 + 2 * Radius + 1}, {y, 200}]),

                  ok
              end,
              ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
              ws_client_sel_recv(ws_control_client, <<"StartGameAck">>),
              ws_client_sel_recv(ws_control_client, <<"GameSnapshotNotification">>),
              fun (Context) ->
                  #{pewpew_game := PewPewGame} = Context,
                  ArenaComponent = pewpew_game:arena_component(PewPewGame),
                  Players = pewpew_arena_component:players(ArenaComponent),
                  ExpectedPlayerStates = lists:map(fun(Player) ->
                          #{
                            <<"id">> => pewpew_player_component:id(Player),
                            <<"coordinates">> => #{
                              <<"x">> => pewpew_player_component:x(Player),
                              <<"y">> => pewpew_player_component:y(Player)
                              },
                            <<"life">> => pewpew_player_component:life(Player),
                            <<"rotation">> => pewpew_player_component:rotation(Player),
                            <<"radar">> => #{<<"type">> => <<"circular_scan">>, <<"radius">> => 40}
                            }
                      end, Players),

                  ExpectedReply = #{
                      <<"type">> => <<"GameSnapshotNotification">>,
                      <<"data">> => #{
                        <<"arena_snapshot">> => #{
                          <<"players_snapshots">> => ExpectedPlayerStates,
                          <<"shots_snapshots">> => []
                          }
                        }
                      },

                  test_step(validate_message_in_last_reply_matches(ws_control_client, ExpectedReply))
              end,
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
              ws_client_sel_recv(ws_player_2_client, <<"PlayerDestroyedNotification">>),
              ws_client_sel_recv(ws_control_client, <<"GameSnapshotNotification">>)
              ],

            test => fun(Context) ->
                Player1 = get_player_for_client(ws_player_1_client, Context),
                ExpectePlayerState = #{
                    <<"id">> => pewpew_player_component:id(Player1),
                    <<"coordinates">> => #{
                      <<"x">> => pewpew_player_component:x(Player1),
                      <<"y">> => pewpew_player_component:y(Player1)
                      },
                    <<"life">> => pewpew_player_component:life(Player1),
                    <<"rotation">> => pewpew_player_component:rotation(Player1),
                    <<"radar">> => #{<<"type">> => <<"circular_scan">>, <<"radius">> => 40}
                    },

                ExpectedReply = #{
                    <<"type">> => <<"GameSnapshotNotification">>,
                    <<"data">> => #{
                      <<"arena_snapshot">> => #{
                        <<"players_snapshots">> => [ExpectePlayerState],
                        <<"shots_snapshots">> => []
                        }
                      }
                    },

                validate_message_in_last_reply_matches(ws_control_client, ExpectedReply)
            end
            })
      },
      {"Shots are included in game snapshot notifications",
       run_test(#{
            steps => [
              register_player(ws_player_1_client),
              ws_client_sel_recv(ws_player_1_client, <<"RegisterPlayerAck">>),
              place_player_at(ws_player_1_client, [{x, 200}, {y, 200}]),
              ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
              ws_client_sel_recv(ws_player_1_client, <<"StartGameOrder">>),
              fun (_) ->
                  Steps = lists:seq(1, 1),
                  lists:map(fun(_) ->
                        fun(_) ->
                            [
                              ws_client_send(ws_player_1_client, #{type => <<"PlayerShootCommand">>, data => #{}}),
                              ws_client_sel_recv(ws_player_1_client, <<"PlayerShootAck">>)
                              ]
                        end
                    end, Steps)
              end,
              ws_client_sel_recv(ws_control_client, <<"GameSnapshotNotification">>)
              ],

            test => fun(Context) ->
                #{pewpew_game := PewPewGame} = Context,
                ArenaComponent = pewpew_game:arena_component(PewPewGame),
                [Shot] = pewpew_arena_component:shots(ArenaComponent),
                ExpectedShotState = #{
                    <<"coordinates">> => #{
                      <<"x">> => pewpew_shot_component:x(Shot),
                      <<"y">> => pewpew_shot_component:y(Shot)
                      },
                    <<"id">> => pewpew_shot_component:id(Shot)
                    },

                ExpectedReply = #{
                    <<"type">> => <<"GameSnapshotNotification">>,
                    <<"data">> => #{<<"arena_snapshot">> => #{ <<"shots_snapshots">> => [ExpectedShotState]}}
                    },

                validate_message_in_last_reply_matches(ws_control_client, ExpectedReply)
            end
            })
      },
      {"Shots are not included in game snapshot notifications after being destroyed",
       run_test(#{
            steps => [
              register_player(ws_player_1_client),
              ws_client_sel_recv(ws_player_1_client, <<"RegisterPlayerAck">>),
              place_player_at(ws_player_1_client, [{x, 799}, {y, 200}]),
              ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
              ws_client_sel_recv(ws_player_1_client, <<"StartGameOrder">>),
              ws_client_send(ws_player_1_client, #{type => <<"PlayerShootCommand">>, data => #{}}),
              ws_client_sel_recv(ws_player_1_client, <<"PlayerShootAck">>),
              ws_client_sel_recv(ws_control_client, <<"GameSnapshotNotification">>),
              fun (Context) ->
                #{pewpew_game := PewPewGame} = Context,
                ArenaComponent = pewpew_game:arena_component(PewPewGame),
                [Shot] = pewpew_arena_component:shots(ArenaComponent),
                ExpectedShotState = #{
                    <<"coordinates">> => #{
                      <<"x">> => pewpew_shot_component:x(Shot),
                      <<"y">> => pewpew_shot_component:y(Shot)
                      },
                      <<"id">> => pewpew_shot_component:id(Shot)
                    },

                ExpectedReply = #{
                    <<"type">> => <<"GameSnapshotNotification">>,
                    <<"data">> => #{<<"arena_snapshot">> => #{ <<"shots_snapshots">> => [ExpectedShotState]}}
                    },

                validate_message_in_last_reply_matches(ws_control_client, ExpectedReply)
              end,
              ws_client_sel_recv(ws_control_client, <<"GameSnapshotNotification">>)
              ],

            test => fun(_) ->
                ExpectedReply = #{
                    <<"type">> => <<"GameSnapshotNotification">>,
                    <<"data">> => #{<<"arena_snapshot">> => #{ <<"shots_snapshots">> => []}}
                    },

                validate_message_in_last_reply_matches(ws_control_client, ExpectedReply)
            end
            })
      },
      {"Shooting decreases the tokens count and every cycle increases it",
       run_test(#{
            steps => [
              register_player(),
              ws_client_sel_recv(ws_player_client, <<"RegisterPlayerAck">>),
              ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
              ws_client_sel_recv(ws_player_client, <<"StartGameOrder">>),
              fun (Context) ->
                  Player = get_player_for_client(ws_player_client, Context),
                  ShootingInfo = pewpew_player_component:shooting_info(Player),
                  #{
                    cost := ShootingCost,
                    tokens := ShootingTokens,
                    new_tokens_per_cycle := NewTokensPerCycle
                    } = ShootingInfo,

                  % NOTE
                  % The equation below is only valid when the max_tokens and initial_tokens config values are equal.
                  % On this case the first attemp to increase the tokens value will be rejected and thats why we start with
                  % (ShootingTokens - ShootingCost) and add an extra initial (+ 1) shot
                  MaxShots = trunc((ShootingTokens - ShootingCost) / (ShootingCost - NewTokensPerCycle)) + 1,

                  Steps = lists:seq(1, MaxShots),
                  lists:map(fun(Index) ->
                        fun(_) ->
                            [
                              fun (InnerContext) ->
                                  Player = get_player_for_client(ws_player_client, InnerContext),
                                  InnerShootingInfo = pewpew_player_component:shooting_info(Player),
                                  #{cost := Cost, tokens := Tokens, new_tokens_per_cycle := InnerNewTokensPerCycle} = InnerShootingInfo,

                                  UpdatedContext = lists:foldl(fun ({Key, Value}, Acc) ->
                                          maps:put(Key, Value, Acc)
                                      end, InnerContext, [
                                        {shooting_cost, Cost},
                                        {shooting_tokens, Tokens},
                                        {new_tokens_per_cycle, InnerNewTokensPerCycle}
                                        ]),

                                  {context, UpdatedContext}
                              end,
                              ws_client_send(ws_player_client, #{type => <<"PlayerShootCommand">>, data => #{}}),
                              ws_client_sel_recv(ws_player_client, <<"PlayerShootAck">>),
                              test_step(fun (InnerContext) ->
                                    Message = get_message_in_last_reply_for_client(ws_player_client, <<"PlayerShootAck">>, InnerContext),
                                    #{
                                      shooting_cost := InnerShootingCost,
                                      shooting_tokens := InnerShootingTokens,
                                      new_tokens_per_cycle := InnerNewTokensPerCycle
                                      } = InnerContext,

                                    ExpectedTokens = case Index of
                                      1 -> InnerShootingTokens - InnerShootingCost;
                                      _ -> InnerShootingTokens - InnerShootingCost + InnerNewTokensPerCycle
                                    end,

                                    ExpectedMessage = #{
                                        <<"data">> => #{
                                          <<"shooting">> => #{
                                            <<"tokens">> => ExpectedTokens
                                            }
                                          }
                                        },

                                    validate_message_matches(Message, ExpectedMessage)
                                end)
                              ]
                        end
                    end, Steps)
              end
              ]
            })
      },
      {"Shooting tokens are not increased over a limit",
       run_test(#{
            steps => [
              register_player(),
              ws_client_sel_recv(ws_player_client, <<"RegisterPlayerAck">>),
              ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
              ws_client_sel_recv(ws_player_client, <<"StartGameOrder">>),
              % wait some cycles
              fun (_) ->
                  MaxShootingTokens = pewpew_config:get([players, shooting, max_tokens]),
                  InitialTokens     = pewpew_config:get([players, shooting, initial_tokens]),
                  NewTokensPerCycle = pewpew_config:get([players, shooting, new_tokens_per_cycle]),

                  CyclesToTokensParity = pewpew_utils:ceil((MaxShootingTokens - InitialTokens) / NewTokensPerCycle),

                  % Add some extra cycles to ensure that after parity the number of tokens
                  % remain stable
                  Steps = lists:seq(1, CyclesToTokensParity + 5),
                  lists:map(fun(_) ->
                        fun(_) ->
                              [
                                ws_client_sel_recv(ws_control_client, <<"GameSnapshotNotification">>)
                                ]
                        end
                    end, Steps)
              end,
              ws_client_send(ws_player_client, #{<<"type">> => <<"PlayerShootCommand">>, <<"data">> => #{}}),
              ws_client_sel_recv(ws_player_client, <<"PlayerShootAck">>)
              ],

            test => fun(_) ->
                MaxTokens = pewpew_config:get([players, shooting, max_tokens]),
                ShotCost = pewpew_config:get([players, shooting, cost]),

                ExpectedTokens = MaxTokens - ShotCost,

                ExpectedMessage = #{
                    <<"data">> => #{
                      <<"shooting">> => #{
                        <<"tokens">> => ExpectedTokens
                        }
                      }
                    },

                validate_message_in_last_reply_matches(ws_player_client, ExpectedMessage)
            end
            })
      },
      {"Players can't shoot when they don't have enough shooting tokens",
       run_test(#{
            steps => [
              register_player(),
              ws_client_sel_recv(ws_player_client, <<"RegisterPlayerAck">>),
              ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
              ws_client_sel_recv(ws_player_client, <<"StartGameOrder">>),
              fun (Context) ->
                  Player = get_player_for_client(ws_player_client, Context),
                  ShootingInfo = pewpew_player_component:shooting_info(Player),
                  #{
                    cost := ShootingCost,
                    new_tokens_per_cycle := NewTokensPerCycle
                    } = ShootingInfo,
                  shoot_until_out_of_tokens(ShootingCost - NewTokensPerCycle)
              end,
              ws_client_send(ws_player_client, #{type => <<"PlayerShootCommand">>, data => #{}}),
              ws_client_recv(ws_player_client),
              test_step(validate_type_in_last_reply_test(ws_player_client, <<"InvalidCommandError">>))
              ]
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

shoot_until_out_of_tokens(Delta) ->
  fun(_) ->
      [
        ws_client_send(ws_player_client, #{type => <<"PlayerShootCommand">>, data => #{}}),
        ws_client_sel_recv(ws_player_client, <<"PlayerShootAck">>),
        fun(Context) ->
            Message = get_message_in_last_reply_for_client(ws_player_client, <<"PlayerShootAck">>, Context),
            CurrentTokens = pewpew_utils:get_value_in_map([<<"data">>, <<"shooting">>, <<"tokens">>], Message),

            case CurrentTokens < Delta of
              true -> ok;
              false -> shoot_until_out_of_tokens(Delta)
            end
        end
        ]
  end.
