-module(pewpew_game_snapshot_notification_tests).
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
    get_player_for_client/2,
    validate_message_in_last_reply_test/2
]).

tests() ->
  {"GameSnapshotNotification",[
          {"It is sent to the control client",
            run_test(#{
              steps => [ws_client_recv(ws_control_client)],

              test => validate_message_in_last_reply_test(
                ws_control_client,
                #{
                  <<"type">> => <<"GameSnapshotNotification">>,
                  <<"data">> => #{<<"arena_snapshot">> => #{ <<"players_snapshots">> => [] }}
                })
             })
          },
          {"It includes registered players",
            run_test(#{
              steps => [
                register_player(),
                ws_client_sel_recv(ws_player_client, <<"RegisterPlayerAck">>),
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
                  <<"radar">> => #{<<"type">> => <<"circular_scan">>, <<"radius">> => 40}
                 },

                ExpectedReply = #{
                  <<"type">> => <<"GameSnapshotNotification">>,
                  <<"data">> => #{<<"arena_snapshot">> => #{ <<"players_snapshots">> => [ExpectedPlayerState]}}
                },

                validate_message_in_last_reply_test(ws_control_client, ExpectedReply)
              end
             })
          },
          {"It reflects players movements",
            run_test(#{
              steps => [
                register_player(),
                ws_client_sel_recv(ws_player_client, <<"RegisterPlayerAck">>), % ensure the player is registered
                ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
                ws_client_sel_recv(ws_player_client, <<"StartGameOrder">>),
                ws_client_sel_recv(ws_control_client, <<"StartGameAck">>),
                ws_client_send(ws_player_client, #{type => <<"MovePlayerCommand">>, data => [#{move => forward}] }),
                ws_client_recv(ws_control_client)
              ],

              test => fun(Context) ->
                Player = get_player_for_client(ws_player_client, Context),
                ExpectedPlayerState = #{
                  <<"id">> => pewpew_player_component:id(Player),
                  <<"coordinates">> => #{
                      <<"x">> => pewpew_player_component:x(Player),
                      <<"y">> => pewpew_player_component:y(Player)
                  },
                  <<"life">> => pewpew_player_component:life(Player),
                  <<"rotation">> => pewpew_player_component:rotation(Player),
                  <<"radar">> => #{<<"type">> => <<"circular_scan">>, <<"radius">> => 40}
                 },

                ExpectedReply = #{
                  <<"type">> => <<"GameSnapshotNotification">>,
                  <<"data">> => #{<<"arena_snapshot">> => #{ <<"players_snapshots">> => [ExpectedPlayerState]}}
                },

                validate_message_in_last_reply_test(ws_control_client, ExpectedReply)
              end
             })
          }
        ]}.
