-module(pewpew_player_registration_tests).
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
    get_last_reply_for_client/2,
    validate_type_in_last_reply_test/2,
    throwing/1,
    it_threw/1,
    get_player_for_client/2,
    is_ws_client_dead/2
    ]).

tests() ->
  {"RegisterPlayerCommand", [
      {"Register player command",
       run_test(#{
            steps => [
              register_player(),
              ws_client_recv(ws_player_client)
              ],

            test => fun(Context) ->
                JSON = get_last_reply_for_client(ws_player_client, Context),
                Player = get_player_for_client(ws_player_client, Context),

                [
                  #{<<"type">> := AckType, <<"data">> := Data}
                ] = JSON,
                #{
                  <<"id">> := Id,
                  <<"x">> := X,
                  <<"y">> := Y,
                  <<"life">> := Life,
                  <<"shooting">> := Shooting
                } = Data,

                #{
                  <<"cost">> := ShootingShotCost,
                  <<"tokens">> := ShootingShotTokens
                } = Shooting,

                PlayerShootingInfo = pewpew_player_component:shooting_info(Player),

                #{
                  cost := PlayerShootingShotCost,
                  tokens := PlayerShootingShotTokens
                 } = PlayerShootingInfo,

                [
                  ?_assertEqual(<<"RegisterPlayerAck">>, AckType),
                  ?_assert(is_integer(Id)),
                  ?_assert(is_integer(X)),
                  ?_assert(is_integer(Y)),
                  ?_assert(is_integer(Life)),
                  ?_assertEqual(PlayerShootingShotCost, ShootingShotCost),
                  ?_assertEqual(PlayerShootingShotTokens, ShootingShotTokens)
                ]
            end
            })
      },
      {"It registers multiple players",
       run_test(#{
            steps => fun(_Context) ->
                [
                  register_player(ws_player_1_client),
                  register_player(ws_player_2_client),
                  ws_client_sel_recv(ws_player_1_client, <<"RegisterPlayerAck">>),
                  ws_client_sel_recv(ws_player_2_client, <<"RegisterPlayerAck">>)
                  ]
            end,

            test => fun(Context) ->
                #{ pewpew_game := Game } = Context,
                ArenaComponent = pewpew_game:arena_component(Game),
                NPlayers = length(pewpew_arena_component:players(ArenaComponent)),

                [?_assertEqual(2, NPlayers)]
            end
            })
      },
      {"It rejects player registering twice",
       run_test(#{
            steps => [
              register_player(),
              ws_client_sel_recv(ws_player_client, <<"RegisterPlayerAck">>),
              ws_client_send(ws_player_client, #{type => <<"RegisterPlayerCommand">>, data => #{}}),
              ws_client_recv(ws_player_client)
              ],

            test => validate_type_in_last_reply_test(ws_player_client, <<"InvalidCommandError">>)
            })
      },
      {"It registers player after game has started",
       run_test(#{
            steps => [
              ws_client_send(ws_control_client, <<"{\"type\":\"StartGameCommand\", \"data\":{}}">>),
              ws_client_sel_recv(ws_control_client, <<"StartGameAck">>),
              ws_client_send(ws_player_client, #{type => <<"RegisterPlayerCommand">>, data => #{}}),
              ws_client_recv(ws_player_client)
              ],
            test => validate_type_in_last_reply_test(ws_player_client, <<"RegisterPlayerAck">>)
            })
      },
      {"It does not not receive StartGameOrder when registering after game started",
       run_test(#{
            steps => [
              ws_client_send(ws_control_client, <<"{\"type\":\"StartGameCommand\", \"data\":{}}">>),
              ws_client_sel_recv(ws_control_client, <<"StartGameAck">>),
              ws_client_send(ws_player_client, #{type => <<"RegisterPlayerCommand">>, data => #{}}),
              throwing(ws_client_sel_recv(ws_player_client, <<"StartGameOrder">>))
              ],
            test => it_threw(ws_client_sel_recv_timeout)
            })
      },
      {"It receives StartGameOrder when registering after game started",
       run_test(#{
            steps => [
              fun(_) ->
                  % This config property should be set per Game instance
                  % but with the current code only one Game is possible
                  % and that Game is created when starting the server.
                  pewpew_config:set(players_can_join_started_game, true)
              end,
              ws_client_send(ws_control_client, <<"{\"type\":\"StartGameCommand\", \"data\":{}}">>),
              ws_client_sel_recv(ws_control_client, <<"StartGameAck">>),
              ws_client_send(ws_player_client, #{type => <<"RegisterPlayerCommand">>, data => #{}}),
              ws_client_recv(ws_player_client)
              ],
            test => validate_type_in_last_reply_test(ws_player_client, <<"StartGameOrder">>)
            })
      },
      {"It sends a notification when the are no free slots",
       focus,
       run_test(#{
            steps => [
              register_player(ws_player_1_client),
              register_player(ws_player_2_client),
              register_player(ws_player_3_client),
              ws_client_sel_recv(ws_player_1_client, <<"RegisterPlayerAck">>),
              ws_client_sel_recv(ws_player_2_client, <<"RegisterPlayerAck">>),
              ws_client_sel_recv(ws_player_3_client, <<"RegisterPlayerAck">>),
              register_player(ws_player_4_client),
              ws_client_recv(ws_player_4_client)
              ],

            test => fun (Context) ->
                timer:sleep(100), % buy some time for the client to shutdown
                IsLastWSClientDead = is_ws_client_dead(ws_player_4_client, Context),

                [
                  ?_assert(IsLastWSClientDead),
                  validate_type_in_last_reply_test(ws_player_4_client, <<"NoSlotsLeftNotification">>)
                ]
            end
            })
      }
      ]}.
