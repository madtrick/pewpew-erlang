-module(pewpew_game_start_tests).
-include_lib("eunit/include/eunit.hrl").

-export([tests/0]).
-import(
  pewpew_test_support, [
    run_test/1,
    register_player/0,
    ws_client_send/2,
    ws_client_recv/1,
    ws_client_sel_recv/2,
    validate_type_in_last_reply_test/2
]).

tests() ->
{"StartGameCommand", [
          {"Gets an StartGameAck back",
            run_test(#{
              steps => [
                ws_client_send(ws_control_client, <<"{\"type\":\"StartGameCommand\", \"data\":{}}">>),
                ws_client_sel_recv(ws_control_client, <<"StartGameAck">>)
              ],

              test => ?_assert(true)
           })
          },
           {"Rejects command when the origin is invalid",
            run_test(#{
              steps => [
                ws_client_send(ws_player_client, <<"{\"type\":\"StartGameCommand\", \"data\":{}}">>),
                ws_client_recv(ws_player_client)
              ],

              test => validate_type_in_last_reply_test(ws_player_client, <<"InvalidCommandError">>)
             })
           },
           {"Rejects command when game has already been started",
            run_test(#{
              steps => [
                ws_client_send(ws_control_client, <<"{\"type\":\"StartGameCommand\", \"data\":{}}">>),
                ws_client_sel_recv(ws_control_client, <<"StartGameAck">>),
                ws_client_send(ws_control_client, <<"{\"type\":\"StartGameCommand\", \"data\":{}}">>),
                ws_client_sel_recv(ws_control_client, <<"InvalidCommandError">>)
              ],

              test => ?_assert(true)
             })
           },
           {"StartGameOrder is sent to players",
            run_test(#{
              steps => [
                register_player(),
                ws_client_recv(ws_player_client),
                ws_client_send(ws_control_client, <<"{\"type\":\"StartGameCommand\", \"data\":{}}">>),
                ws_client_recv(ws_player_client)
              ],

              test => validate_type_in_last_reply_test(ws_player_client, <<"StartGameOrder">>)
             })
           }
        ]}.
