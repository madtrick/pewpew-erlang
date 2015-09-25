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
    generate_reject_move_command_test/1,
    generate_valid_move_command_test/1,
    register_player/0,
    register_player/1,
    validate_type_in_last_reply_test/2,
    place_player_at/2
]).

tests() ->
  {"MovePlayerCommand", [
            {"Rejects command when game is already started",
              run_test(#{
                steps => [
                    register_player(),
                    ws_client_recv(ws_player_client),
                    ws_client_send(ws_player_client, <<"{\"type\":\"MovePlayerCommand\", \"data\":[]}">>),
                    ws_client_recv(ws_player_client)
                  ],

                test => validate_type_in_last_reply_test(ws_player_client, <<"InvalidCommandError">>)
               })
            },
            {"Rejects command when the player is not registered",
              run_test(#{
                steps => [
                    ws_client_send(ws_control_client, <<"{\"type\":\"StartGameCommand\", \"data\":{}}">>),
                    ws_client_send(ws_player_client, <<"{\"type\":\"MovePlayerCommand\", \"data\":[]}">>),
                    ws_client_recv(ws_player_client)
                  ],

                test => validate_type_in_last_reply_test(ws_player_client, <<"InvalidCommandError">>)
               })
            },
            {"Rejects command when the player hits arena edges",
              generate_reject_move_command_test(#{
                coordinates => fun(ArenaWidth, _) -> [{x, ArenaWidth}] end,
                movements => [#{move => forward}]
               })
            },
            {"Rejects command when the player hits arena edges (not on the edge)",
              generate_reject_move_command_test(#{
                coordinates => fun(ArenaWidth, _) -> [{x, ArenaWidth - 2}] end,
                movements => [#{move => forward}]
               })
            },
            {"Rejects command when the player hits arena edges (negative edges)",
              generate_reject_move_command_test(#{
                coordinates => [{x, 5}, {y, 6}],
                movements => [#{move => backward}]
               })
            },
            {"Rejects command when direction is invalid",
              generate_reject_move_command_test(#{
                movements => [#{move => invalid_movement}]
               })
            },
            {"Rejects command when rotation is invalid",
              generate_reject_move_command_test(#{
                movements => [#{rotate => 900}]
               })
            },
            {"Rejects command when includes more than one rotation",
              generate_reject_move_command_test(#{
                movements => [#{rotate => 2}, #{rotate => 2}]
               })
            },
            {"Rejects command when includes more than one movement",
              generate_reject_move_command_test(#{
                movements => [#{move => forward}, #{move => forward}]
               })
            },
            {"Rejects command when it collides with another player",
              run_test(#{
                steps => [
                   register_player(ws_player_1_client),
                   register_player(ws_player_2_client),
                   ws_client_sel_recv(ws_player_1_client, <<"RegisterPlayerAck">>),
                   ws_client_sel_recv(ws_player_2_client, <<"RegisterPlayerAck">>),
                   place_player_at(ws_player_1_client, [{x, 200}, {y, 200}]),
                   place_player_at(ws_player_2_client, [{x, 206}, {y, 200}]),
                   ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
                   ws_client_sel_recv(ws_player_1_client, <<"StartGameOrder">>),
                   ws_client_send(ws_player_1_client, #{type => <<"MovePlayerCommand">>, data => [#{move => <<"forward">>}]}),
                   ws_client_sel_recv(ws_player_1_client, <<"InvalidCommandError">>)
               ],
                test => ?_assert(true)
               })
            },
            {"Accepts valid movements",
              fun () ->
                Coordinates = fun(_, _) -> [{x, 10}, {y, 10}] end,
                Movements = [
                 #{
                  coordinates => Coordinates,
                  movements => [#{rotate => 60}, #{move => forward}],
                  expectations => #{x => 10.5, y =>10.86603}
                  },
                 #{
                  coordinates => Coordinates,
                  movements => [#{rotate => 60}],
                  expectations => #{x => 10, y =>10}
                  },
                 #{
                  coordinates => Coordinates,
                  movements => [#{move => forward}],
                  expectations => #{x => 11.0, y =>10.0}
                  },
                 #{
                  coordinates => Coordinates,
                  movements => [#{move => backward}],
                  expectations => #{x => 9.0, y =>10.0}
                  }
                ],

                lists:map(fun(Movement) -> generate_valid_move_command_test(Movement) end, Movements)
              end
            }
          ]}.
