-module(pewpew_misc_tests).
-include_lib("eunit/include/eunit.hrl").

-export([tests/0]).
-import(
  pewpew_test_support, [
    run_test/1,
    ws_client_send/2,
    ws_client_recv/1,
    test_step/1,
    validate_type_in_last_reply_test/2
    ]).

tests() ->
  {"Misc tests", [
      {"It handles commands without data",
       run_test(#{
            steps =>  [
              ws_client_send(ws_player_client, <<"{\"type\":\"RegisterPlayerCommand\"}">>),
              ws_client_recv(ws_player_client),
              test_step(validate_type_in_last_reply_test(ws_player_client, <<"RegisterPlayerAck">>))
              ]
            })
      },
      {"It returns an error when the message has no type property",
       run_test(#{
            steps => [
              ws_client_send(ws_player_client, <<"\"{\"data\": 123}\"">>),
              ws_client_recv(ws_player_client),
              test_step(validate_type_in_last_reply_test(ws_player_client, <<"InvalidCommandError">>))
              ]
            })
      },
      {"It returns an error when the message is truncated",
       run_test(#{
            steps => [
              ws_client_send(ws_player_client, <<"\"{\"data\"">>),
              ws_client_recv(ws_player_client),
              test_step(validate_type_in_last_reply_test(ws_player_client, <<"InvalidCommandError">>))
              ]
            })
      },
      {"It returns an error when the payload is not JSON",
       run_test(#{
            steps => [
              ws_client_send(ws_player_client, <<"\"\"">>),
              ws_client_recv(ws_player_client),
              test_step(validate_type_in_last_reply_test(ws_player_client, <<"InvalidCommandError">>))
              ]
            })
      }
      ]}.
