-module(pewpew_radar_scan_notification_tests).
-include_lib("eunit/include/eunit.hrl").

-export([tests/0]).
-import(
  pewpew_test_support, [
    run_test/1,
    ws_client_send/2,
    ws_client_recv/1,
    ws_client_sel_recv/2,
    ws_client_sel_recv/2,
    register_player/0,
    register_player/1,
    validate_type_in_last_reply_test/2,
    get_player_for_client/2,
    validate_message_in_last_reply_test/2,
    validate_message_in_nth_reply_test/3
]).

tests() ->
{"RadaScanNotification", [
          % TODO: add test to verify that no notification is sent before the game starts
          {"Its sent after the game has started",
            run_test(#{
              steps => [
                register_player(),
                ws_client_sel_recv(ws_player_client, <<"RegisterPlayerAck">>),
                ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
                ws_client_sel_recv(ws_player_client, <<"StartGameOrder">>),
                ws_client_recv(ws_player_client)
              ],

              test => validate_type_in_last_reply_test(ws_player_client, <<"RadarScanNotification">>)
            })
          },
          {"It includes detected players",
            run_test(#{
              steps => fun(_Context) ->
                [
                 register_player(ws_player_1_client),
                 register_player(ws_player_2_client),
                 ws_client_sel_recv(ws_player_1_client, <<"RegisterPlayerAck">>),
                 ws_client_sel_recv(ws_player_2_client, <<"RegisterPlayerAck">>),
                 fun(Context) ->
                     ScanningPlayer = get_player_for_client(ws_player_1_client, Context),
                     ScannedPlayer  = get_player_for_client(ws_player_2_client, Context),

                     % center the player to avoid the walls
                     pewpew_player_component:set_coordinates(ScanningPlayer, [{x, 200}, {y, 200}]),
                     pewpew_player_component:set_coordinates(ScannedPlayer, [{x, 220}, {y, 220}]),

                     {context, Context}
                 end,
                 ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
                 ws_client_sel_recv(ws_player_1_client, <<"StartGameOrder">>),
                 ws_client_sel_recv(ws_player_2_client, <<"StartGameOrder">>),
                 ws_client_recv(ws_player_1_client),
                 ws_client_recv(ws_player_2_client)
                ]
              end,

              test => fun(_) ->
                ScannedPlayersExpectations = [
                  {ws_player_1_client,
                    #{
                      <<"coordinates">> => #{<<"x">> => 220, <<"y">> => 220},
                      <<"type">> => <<"unknown">>
                    }
                  },
                  {ws_player_2_client,
                    #{
                      <<"coordinates">> => #{<<"x">> => 200, <<"y">> => 200},
                      <<"type">> => <<"unknown">>
                    }
                  }
                ],

                lists:map(fun({ClientId, ScannedPlayerExpectation}) ->
                  ExpectedReply = #{
                    <<"type">> => <<"RadarScanNotification">>,
                    <<"data">> => #{
                        <<"elements">> => [ScannedPlayerExpectation],
                        <<"walls">> => []
                       }
                  },

                  validate_message_in_last_reply_test(ClientId, ExpectedReply)
                end, ScannedPlayersExpectations)
              end
             })
          },
          {"It does not include non detected players",
            run_test(#{
              steps => fun(_Context) ->
                [
                 register_player(ws_player_1_client),
                 register_player(ws_player_2_client),
                 ws_client_sel_recv(ws_player_1_client, <<"RegisterPlayerAck">>),
                 ws_client_sel_recv(ws_player_2_client, <<"RegisterPlayerAck">>),
                 fun(Context) ->
                     ScanningPlayer = get_player_for_client(ws_player_1_client, Context),
                     ScannedPlayer  = get_player_for_client(ws_player_2_client, Context),

                     % center the player to avoid the walls
                     pewpew_player_component:set_coordinates(ScanningPlayer, [{x, 200}, {y, 200}]),
                     pewpew_player_component:set_coordinates(ScannedPlayer, [{x, 250}, {y, 250}]),

                     {context, Context}
                 end,
                 ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
                 ws_client_sel_recv(ws_player_1_client, <<"StartGameOrder">>),
                 ws_client_recv(ws_player_1_client),
                 ws_client_recv(ws_player_2_client)
                ]
              end,

              test => fun(_) ->
                CliendIds = [ ws_player_1_client, ws_player_2_client],

                lists:map(fun(ClientId) ->
                  ExpectedReply = #{
                    <<"type">> => <<"RadarScanNotification">>,
                    <<"data">> => #{
                        <<"elements">> => [],
                        <<"walls">> => []
                       }
                  },

                  validate_message_in_last_reply_test(ClientId, ExpectedReply)
                end, CliendIds)
              end
             })
          },
          {"It detects walls (circular radar)",
            run_test(#{
              steps => fun(_Context) ->
                [
                 register_player(ws_player_1_client),
                 ws_client_sel_recv(ws_player_1_client, <<"RegisterPlayerAck">>),
                 fun(Context) ->
                     ScanningPlayer = get_player_for_client(ws_player_1_client, Context),

                     % center the player to avoid the walls
                     pewpew_player_component:set_coordinates(ScanningPlayer, [{x, 380}, {y, 380}]),

                     {context, Context}
                 end,
                 ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
                 ws_client_sel_recv(ws_player_1_client, <<"StartGameOrder">>),
                 ws_client_recv(ws_player_1_client)
                ]
              end,

              test => fun(_) ->
                  ExpectedReply = #{
                    <<"type">> => <<"RadarScanNotification">>,
                    <<"data">> => #{
                        <<"elements">> => [],
                        <<"walls">> => [[[414, 400], [345, 400]]]
                       }
                  },

                  validate_message_in_last_reply_test(ws_player_1_client, ExpectedReply)
              end
             })
          },
          {"It rejects radar configuration before game starts",
            run_test(#{
              steps => fun(_Context) ->
                [
                 register_player(ws_player_1_client),
                 ws_client_sel_recv(ws_player_1_client, <<"RegisterPlayerAck">>),
                 ws_client_send(ws_player_1_client, <<"{\"type\":\"ConfigurePlayerCommand\", \"data\":{\"op\":\"radarType\", \"args\": [\"long_range_scan\"]}}">>),
                 ws_client_recv(ws_player_1_client)
                ]
              end,

              test =>  validate_type_in_last_reply_test(ws_player_1_client, <<"InvalidCommandError">>)
             })
          },
          {"It rejects radar configuration with invalid operations",
            run_test(#{
              steps => fun(_Context) ->
                [
                 register_player(ws_player_1_client),
                 ws_client_sel_recv(ws_player_1_client, <<"RegisterPlayerAck">>),
                 ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
                 ws_client_sel_recv(ws_player_1_client, <<"StartGameOrder">>),
                 ws_client_send(ws_player_1_client, <<"{\"type\":\"ConfigurePlayerCommand\", \"data\":{\"op\":\"lol\", \"args\": [\"bacon\"]}}">>),
                 ws_client_sel_recv(ws_player_1_client, <<"InvalidCommandError">>)
                ]
              end,

              test => ?_assert(true)
             })
          },
          {"It rejects radar configuration with invalid arguments",
            run_test(#{
              steps => fun(_Context) ->
                [
                 register_player(ws_player_1_client),
                 ws_client_sel_recv(ws_player_1_client, <<"RegisterPlayerAck">>),
                 ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
                 ws_client_sel_recv(ws_player_1_client, <<"StartGameOrder">>),
                 ws_client_send(ws_player_1_client, #{type => <<"ConfigurePlayerCommand">>, data => #{op => <<"radarType">>, args => [<<"bacon">>]}}),
                 ws_client_sel_recv(ws_player_1_client, <<"InvalidCommandError">>)
                ]
              end,

              test => ?_assert(true)
             })
          },
          {"It accepts mode change to long radar scan",
            run_test(#{
              steps => fun(_Context) ->
                [
                 register_player(ws_player_1_client),
                 ws_client_sel_recv(ws_player_1_client, <<"RegisterPlayerAck">>),
                 ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
                 ws_client_sel_recv(ws_player_1_client, <<"StartGameOrder">>),
                 ws_client_send(ws_player_1_client, #{type => <<"ConfigurePlayerCommand">>, data => #{ op => <<"radarType">>, args => [<<"long_range_scan">>] }}),
                 ws_client_sel_recv(ws_player_1_client, <<"ConfigurePlayerAck">>)
                ]
              end,

              test =>  ?_assert(true)
             })
          },
          {"GameSnapshotNotification includes radar mode change",
            run_test(#{
              steps => [
                register_player(),
                ws_client_sel_recv(ws_player_client, <<"RegisterPlayerAck">>),
                ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
                ws_client_sel_recv(ws_player_client, <<"StartGameOrder">>),
                ws_client_send(ws_player_client, #{type => <<"ConfigurePlayerCommand">>, data => #{ op => <<"radarType">>, args => [<<"long_range_scan">>] }}),
                ws_client_sel_recv(ws_player_client, <<"ConfigurePlayerAck">>),
                ws_client_recv(ws_control_client)
              ],

              test => fun(Context) ->
                Player              = get_player_for_client(ws_player_client, Context),
                ExpectedPlayerState = #{
                  <<"id">> => pewpew_player_component:id(Player),
                  <<"coordinates">> => #{
                      <<"x">> => pewpew_player_component:x(Player),
                      <<"y">> => pewpew_player_component:y(Player)
                  },
                  <<"life">> => pewpew_player_component:life(Player),
                  <<"rotation">> => pewpew_player_component:rotation(Player),
                  <<"radar">> => #{<<"type">> => <<"long_range_scan">>, <<"radius">> => 80}
                 },

                ExpectedReply = #{
                  <<"type">> => <<"GameSnapshotNotification">>,
                  <<"data">> => #{<<"arena_snapshot">> => #{ <<"players_snapshots">> => [ExpectedPlayerState]}}
                },

                validate_message_in_last_reply_test(ws_control_client, ExpectedReply)
              end
             })
          },
          {"It includes detected players (long range scan)",
            run_test(#{
              steps => fun(_Context) ->
                [
                 register_player(ws_player_1_client),
                 register_player(ws_player_2_client),
                 ws_client_sel_recv(ws_player_1_client, <<"RegisterPlayerAck">>),
                 ws_client_sel_recv(ws_player_2_client, <<"RegisterPlayerAck">>),
                 fun(Context) ->
                     ScanningPlayer = get_player_for_client(ws_player_1_client, Context),
                     ScannedPlayer  = get_player_for_client(ws_player_2_client, Context),

                     % center the player to avoid the walls
                     pewpew_player_component:set_coordinates(ScanningPlayer, [{x, 200}, {y, 200}]),
                     pewpew_player_component:set_coordinates(ScannedPlayer, [{x, 250}, {y, 200}]),

                     {context, Context}
                 end,
                 ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
                 ws_client_sel_recv(ws_player_1_client, <<"StartGameOrder">>),
                 ws_client_send(ws_player_1_client, #{type => <<"ConfigurePlayerCommand">>, data => #{ op => <<"radarType">>, args => [<<"long_range_scan">>] }}),
                 ws_client_sel_recv(ws_player_1_client, <<"ConfigurePlayerAck">>),
                 ws_client_recv(ws_player_1_client),
                 ws_client_recv(ws_player_2_client)
                ]
              end,

              test => fun(_) ->
                  ScannedPlayerExpectation = #{
                      <<"coordinates">> => #{<<"x">> => 250, <<"y">> => 200},
                      <<"type">> => <<"unknown">>
                   },

                  ExpectedReply = #{
                    <<"type">> => <<"RadarScanNotification">>,
                    <<"data">> => #{
                        <<"elements">> => [ScannedPlayerExpectation],
                        <<"walls">> => []
                       }
                  },

                  validate_message_in_last_reply_test(ws_player_1_client, ExpectedReply)
              end
             })
          },
          {"It includes detected players (far players long range scan)",
            run_test(#{
              steps => fun(_Context) ->
                [
                 register_player(ws_player_1_client),
                 register_player(ws_player_2_client),
                 ws_client_sel_recv(ws_player_1_client, <<"RegisterPlayerAck">>),
                 ws_client_sel_recv(ws_player_2_client, <<"RegisterPlayerAck">>),
                 fun(Context) ->
                     ScanningPlayer = get_player_for_client(ws_player_1_client, Context),
                     ScannedPlayer  = get_player_for_client(ws_player_2_client, Context),

                     % center the player to avoid the walls
                     pewpew_player_component:set_coordinates(ScanningPlayer, [{x, 200}, {y, 200}]),
                     pewpew_player_component:set_coordinates(ScannedPlayer, [{x, 290}, {y, 200}]),

                     {context, Context}
                 end,
                 ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
                 ws_client_sel_recv(ws_player_1_client, <<"StartGameOrder">>),
                 ws_client_send(ws_player_1_client, #{type => <<"ConfigurePlayerCommand">>, data => #{ op => <<"radarType">>, args => [<<"long_range_scan">>] }}),
                 ws_client_sel_recv(ws_player_1_client, <<"ConfigurePlayerAck">>),
                 ws_client_recv(ws_player_1_client),
                 ws_client_recv(ws_player_2_client)
                ]
              end,

              test => fun(_) ->
                  ExpectedReply = #{
                    <<"type">> => <<"RadarScanNotification">>,
                    <<"data">> => #{
                        <<"elements">> => [],
                        <<"walls">> => []
                       }
                  },

                  validate_message_in_last_reply_test(ws_player_1_client, ExpectedReply)
              end
             })
          },
          {"It does not include non detected players (long range scan)",
            run_test(#{
              steps => fun(_Context) ->
                [
                 register_player(ws_player_1_client),
                 register_player(ws_player_2_client),
                 ws_client_sel_recv(ws_player_1_client, <<"RegisterPlayerAck">>),
                 ws_client_sel_recv(ws_player_2_client, <<"RegisterPlayerAck">>),
                 fun(Context) ->
                     ScanningPlayer = get_player_for_client(ws_player_1_client, Context),
                     ScannedPlayer  = get_player_for_client(ws_player_2_client, Context),

                     % center the player to avoid the walls
                     pewpew_player_component:set_coordinates(ScanningPlayer, [{x, 200}, {y, 200}]),
                     pewpew_player_component:set_coordinates(ScannedPlayer, [{x, 120}, {y, 200}]),

                     {context, Context}
                 end,
                 ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
                 ws_client_sel_recv(ws_player_1_client, <<"StartGameOrder">>),
                 ws_client_send(ws_player_1_client, #{type => <<"ConfigurePlayerCommand">>, data => #{ op => <<"radarType">>, args => [<<"long_range_scan">>] }}),
                 ws_client_sel_recv(ws_player_1_client, <<"ConfigurePlayerAck">>),
                 ws_client_recv(ws_player_1_client),
                 ws_client_recv(ws_player_2_client)
                ]
              end,

              test => fun(_) ->
                  ExpectedReply = #{
                    <<"type">> => <<"RadarScanNotification">>,
                    <<"data">> => #{
                        <<"elements">> => [],
                        <<"walls">> => []
                       }
                  },

                  validate_message_in_last_reply_test(ws_player_1_client, ExpectedReply)
              end
             })
          },
          {"Destroyed player does not appear in the scan notification in the same cycle when it is destroyed",
            run_test(#{
              steps => [
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
                ws_client_recv(ws_player_1_client)
              ],

              test => fun(Context) ->
                          Player1       = get_player_for_client(ws_player_1_client, Context),
                          Player1Radius = pewpew_player_component:radius(Player1),

                          [
                           validate_message_in_nth_reply_test(-2, ws_player_1_client, #{
                             <<"type">> => <<"RadarScanNotification">>,
                             <<"data">> => #{
                                 <<"elements">> => [
                                                #{
                                                 <<"type">> => <<"unknown">>,
                                                 <<"coordinates">> => #{<<"x">> => 200 + 2 * Player1Radius + 1, <<"y">> => 200}
                                                }],
                                 <<"walls">> => []
                                }
                           }),
                           validate_message_in_last_reply_test(ws_player_1_client, #{
                             <<"type">> => <<"RadarScanNotification">>,
                             <<"data">> => #{<<"elements">> => [], <<"walls">> => []}
                            })
                          ]
              end
             })
          }

  ]}.
