-module(pewpew_tests).
-include_lib("eunit/include/eunit.hrl").

-import(
  pewpew_test_support, [
    run_test/1,
    ws_client_send/2,
    ws_client_recv/1,
    ws_client_flush/1,
    ws_client_sel_recv/2,
    ws_client_sel_recv/2,
    generate_reject_move_command_test/1,
    generate_valid_move_command_test/1,
    register_player/0,
    register_player/1,
    get_player_for_client/2,
    get_last_reply_for_client/2,
    validate_last_reply_type_test/2,
    validate_last_reply_data_test/2,
    validate_last_reply_test/2
]).

%register_player_command_test_() ->
%  run_test(#{
%    steps => [
%      register_player(),
%      ws_client_recv(ws_player_client)
%    ],

%    test => fun(Context) ->
%      JSON = get_last_reply_for_client(ws_player_client, Context),

%      [
%       #{<<"type">> := AckType, <<"data">> := Data}
%      ] = JSON,
%      #{<<"id">> := Id, <<"x">> := X, <<"y">> := Y, <<"life">> := Life} = Data,

%      [
%        ?_assertEqual(<<"RegisterPlayerAck">>, AckType),
%        ?_assert(is_integer(Id)),
%        ?_assert(is_integer(X)),
%        ?_assert(is_integer(Y)),
%        ?_assert(is_integer(Life))
%      ]
%    end
%   }).

%reject_register_player_twice_test_() ->
%  run_test(#{
%    steps => [
%      register_player(),
%      ws_client_sel_recv(ws_player_client, <<"RegisterPlayerAck">>),
%      ws_client_send(ws_player_client, #{type => <<"RegisterPlayerCommand">>, data => #{}}),
%      ws_client_recv(ws_player_client)
%    ],

%    test => validate_last_reply_type_test(ws_player_client, <<"InvalidCommandError">>)
% }).

%start_game_command_test_() ->
%  run_test(#{
%    steps => [
%      ws_client_send(ws_control_client, <<"{\"type\":\"StartGameCommand\", \"data\":{}}">>),
%      ws_client_recv(ws_control_client)
%    ],

%    test => validate_last_reply_type_test(ws_control_client, <<"StartGameAck">>)
% }).

%reject_start_game_command_when_invalid_origin_test_() ->
%  run_test(#{
%    steps => [
%      ws_client_send(ws_player_client, <<"{\"type\":\"StartGameCommand\", \"data\":{}}">>),
%      ws_client_recv(ws_player_client)
%    ],

%    test => validate_last_reply_type_test(ws_player_client, <<"InvalidCommandError">>)
%   }).

%reject_start_game_command_when_already_started_test_() ->
%  run_test(#{
%    steps => [
%      ws_client_send(ws_control_client, <<"{\"type\":\"StartGameCommand\", \"data\":{}}">>),
%      ws_client_sel_recv(ws_control_client, <<"StartGameAck">>),
%      ws_client_send(ws_control_client, <<"{\"type\":\"StartGameCommand\", \"data\":{}}">>),
%      ws_client_recv(ws_control_client)
%    ],

%    test => validate_last_reply_type_test(ws_control_client, <<"InvalidCommandError">>)
%   }).

%ws_client_send_start_game_order_to_players_test_() ->
%  run_test(#{
%    steps => [
%      register_player(),
%      ws_client_recv(ws_player_client),
%      ws_client_send(ws_control_client, <<"{\"type\":\"StartGameCommand\", \"data\":{}}">>),
%      ws_client_recv(ws_player_client)
%    ],

%    test => validate_last_reply_type_test(ws_player_client, <<"StartGameOrder">>)
%   }).

%reject_move_player_command_when_game_not_started_test_() ->
%  run_test(#{
%    steps => [
%        register_player(),
%        ws_client_recv(ws_player_client),
%        ws_client_send(ws_player_client, <<"{\"type\":\"MovePlayerCommand\", \"data\":[]}">>),
%        ws_client_recv(ws_player_client)
%      ],

%    test => validate_last_reply_type_test(ws_player_client, <<"InvalidCommandError">>)
%   }).

%reject_move_player_command_when_the_player_is_not_registered_test_() ->
%  run_test(#{
%    steps => [
%        ws_client_send(ws_control_client, <<"{\"type\":\"StartGameCommand\", \"data\":{}}">>),
%        ws_client_send(ws_player_client, <<"{\"type\":\"MovePlayerCommand\", \"data\":[]}">>),
%        ws_client_recv(ws_player_client)
%      ],

%    test => validate_last_reply_type_test(ws_player_client, <<"InvalidCommandError">>)
%   }).


%reject_move_player_command_when_player_hits_arena_edges_test_() ->
%  generate_reject_move_command_test(#{
%    coordinates => fun(ArenaWidth, _) -> [{x, ArenaWidth}] end,
%    movements => [#{move => forward}]
%   }).

%reject_move_player_command_when_player_hits_arena_edges_include_radius_test_() ->
%  generate_reject_move_command_test(#{
%    coordinates => fun(ArenaWidth, _) -> [{x, ArenaWidth - 2}] end,
%    movements => [#{move => forward}]
%   }).

%reject_move_player_command_when_player_hits_negative_arena_edges_test_() ->
%  generate_reject_move_command_test(#{
%    coordinates => [{x, 6}, {y, 6}],
%    movements => [#{move => backward}]
%   }).

%reject_move_player_command_when_direction_is_invalid_test_() ->
%  generate_reject_move_command_test(#{
%    movements => [#{move => invalid_movement}]
%   }).

%reject_move_player_command_when_rotation_is_invalid_test_() ->
%  generate_reject_move_command_test(#{
%    movements => [#{rotate => 900}]
%   }).

%reject_move_player_command_when_two_rotations_test_() ->
%  generate_reject_move_command_test(#{
%    movements => [#{rotate => 2}, #{rotate => 2}]
%   }).

%reject_move_player_command_when_two_moves_test_() ->
%  generate_reject_move_command_test(#{
%    movements => [#{move => forward}, #{move => forward}]
%   }).

%move_player_test_() ->
%  Coordinates = fun(_, _) -> [{x, 10}, {y, 10}] end,
%  Movements = [
%   #{
%    coordinates => Coordinates,
%    movements => [#{rotate => 60}, #{move => forward}],
%    expectations => #{x => 10.5, y =>10.86602540378444}
%    },
%   #{
%    coordinates => Coordinates,
%    movements => [#{rotate => 60}],
%    expectations => #{x => 10, y =>10}
%    }
%   #{
%    coordinates => Coordinates,
%    movements => [#{move => forward}],
%    expectations => #{x => 11.0, y =>10.0}
%    },
%   #{
%    coordinates => Coordinates,
%    movements => [#{move => backward}],
%    expectations => #{x => 9.0, y =>10.0}
%    }
%  ],

%  lists:map(fun(Movement) -> generate_valid_move_command_test(Movement) end, Movements).

%send_state_to_control_test_() ->
%  ExpectedReply = #{
%    <<"type">> => <<"GameSnapshotNotification">>,
%    <<"data">> => #{<<"arena_snapshot">> => #{ <<"players_snapshots">> => [] }}
%  },

%  run_test(#{
%    steps => [
%      ws_client_recv(ws_control_client)
%    ],

%    test => validate_last_reply_test(ws_control_client, ExpectedReply)
%   }).

%state_update_includes_registered_player_test_() ->
%  run_test(#{
%    steps => [
%      register_player(),
%      ws_client_sel_recv(ws_player_client, <<"RegisterPlayerAck">>),
%      ws_client_recv(ws_control_client)
%    ],

%    test => fun(Context) ->
%      Player              = get_player_for_client(ws_player_client, Context),
%      ExpectedPlayerState = #{
%        <<"id">> => pewpew_player_component:id(Player),
%        <<"coordinates">> => #{ 
%            <<"x">> => pewpew_player_component:x(Player),
%            <<"y">> => pewpew_player_component:y(Player)
%        },
%        <<"life">> => pewpew_player_component:life(Player),
%        <<"rotation">> => pewpew_player_component:rotation(Player)
%       },

%      ExpectedReply = #{
%        <<"type">> => <<"GameSnapshotNotification">>,
%        <<"data">> => #{<<"arena_snapshot">> => #{ <<"players_snapshots">> => [ExpectedPlayerState]}}
%      },

%      validate_last_reply_test(ws_control_client, ExpectedReply)
%    end
%   }).

%state_update_reflects_player_movement_test_() ->
%  run_test(#{
%    steps => [
%      register_player(),
%      ws_client_sel_recv(ws_player_client, <<"RegisterPlayerAck">>), % ensure the player is registered
%      ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
%      ws_client_sel_recv(ws_player_client, <<"StartGameOrder">>),
%      ws_client_sel_recv(ws_control_client, <<"StartGameAck">>),
%      ws_client_send(ws_player_client, #{type => <<"MovePlayerCommand">>, data => [#{move => forward}] }),
%      ws_client_recv(ws_control_client)
%    ],

%    test => fun(Context) ->
%      Player = get_player_for_client(ws_player_client, Context),
%      ExpectedPlayerState = #{
%        <<"id">> => pewpew_player_component:id(Player),
%        <<"coordinates">> => #{
%            <<"x">> => pewpew_player_component:x(Player),
%            <<"y">> => pewpew_player_component:y(Player)
%        },
%        <<"life">> => pewpew_player_component:life(Player),
%        <<"rotation">> => pewpew_player_component:rotation(Player)
%       },

%      ExpectedReply = #{
%        <<"type">> => <<"GameSnapshotNotification">>,
%        <<"data">> => #{<<"arena_snapshot">> => #{ <<"players_snapshots">> => [ExpectedPlayerState]}}
%      },

%      validate_last_reply_test(ws_control_client, ExpectedReply)
%    end
%   }).

%receive_radar_update_test_() ->
%  run_test(#{
%    steps => [
%      register_player(),
%      ws_client_sel_recv(ws_player_client, <<"RegisterPlayerAck">>),
%      ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
%      ws_client_sel_recv(ws_player_client, <<"StartGameOrder">>),
%      ws_client_recv(ws_player_client)
%    ],

%    test => validate_last_reply_type_test(ws_player_client, <<"RadarScanNotification">>)
%  }).

%register_more_that_one_player_test_() ->
%  run_test(#{
%    steps => fun(_Context) ->
%      [
%       register_player(ws_player_1_client),
%       register_player(ws_player_2_client),
%       ws_client_sel_recv(ws_player_1_client, <<"RegisterPlayerAck">>),
%       ws_client_sel_recv(ws_player_2_client, <<"RegisterPlayerAck">>)
%      ]
%    end,

%    test => fun(Context) ->
%      #{ pewpew_game := Game } = Context,
%      ArenaComponent                 = pewpew_game:arena_component(Game),
%      NPlayers = length(pewpew_arena_component:players(ArenaComponent)),

%      [?_assertEqual(2, NPlayers)]
%    end
%   }).

%radar_detect_player_test_() ->
%  run_test(#{
%    steps => fun(_Context) ->
%      [
%       register_player(ws_player_1_client),
%       register_player(ws_player_2_client),
%       ws_client_sel_recv(ws_player_1_client, <<"RegisterPlayerAck">>),
%       ws_client_sel_recv(ws_player_2_client, <<"RegisterPlayerAck">>),
%       fun(Context) ->
%           ScanningPlayer = get_player_for_client(ws_player_1_client, Context),
%           ScannedPlayer  = get_player_for_client(ws_player_2_client, Context),

%           % center the player to avoid the walls
%           pewpew_player_component:set_coordinates(ScanningPlayer, [{x, 200}, {y, 200}]),
%           pewpew_player_component:set_coordinates(ScannedPlayer, [{x, 220}, {y, 220}]),

%           {context, Context}
%       end,
%       ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
%       ws_client_sel_recv(ws_player_1_client, <<"StartGameOrder">>),
%       ws_client_recv(ws_player_1_client),
%       ws_client_recv(ws_player_2_client)
%      ]
%    end,

%    test => fun(_) ->
%      ScannedPlayersExpectations = [
%        {ws_player_1_client,
%          #{
%            <<"coordinates">> => #{<<"x">> => 220, <<"y">> => 220},
%            <<"type">> => <<"unknown">>
%          }
%        },
%        {ws_player_2_client,
%          #{
%            <<"coordinates">> => #{<<"x">> => 200, <<"y">> => 200},
%            <<"type">> => <<"unknown">>
%          }
%        }
%      ],

%      lists:map(fun({ClientId, ScannedPlayerExpectation}) ->
%        ExpectedReply = #{
%          <<"type">> => <<"RadarScanNotification">>,
%          <<"data">> => #{
%              <<"elements">> => [ScannedPlayerExpectation],
%              <<"walls">> => []
%             }
%        },

%        validate_last_reply_test(ClientId, ExpectedReply)
%      end, ScannedPlayersExpectations)
%    end
%   }).

%radar_does_not_detect_player_test() ->
%  run_test(#{
%    steps => fun(_Context) ->
%      [
%       register_player(ws_player_1_client),
%       register_player(ws_player_2_client),
%       ws_client_sel_recv(ws_player_1_client, <<"RegisterPlayerAck">>),
%       ws_client_sel_recv(ws_player_2_client, <<"RegisterPlayerAck">>),
%       fun(Context) ->
%           ScanningPlayer = get_player_for_client(ws_player_1_client, Context),
%           ScannedPlayer  = get_player_for_client(ws_player_2_client, Context),

%           % center the player to avoid the walls
%           pewpew_player_component:set_coordinates(ScanningPlayer, [{x, 200}, {y, 200}]),
%           pewpew_player_component:set_coordinates(ScannedPlayer, [{x, 250}, {y, 250}]),

%           {context, Context}
%       end,
%       ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
%       ws_client_sel_recv(ws_player_1_client, <<"StartGameOrder">>),
%       ws_client_recv(ws_player_1_client),
%       ws_client_recv(ws_player_2_client)
%      ]
%    end,

%    test => fun(_) ->
%      CliendIds = [ ws_player_1_client, ws_player_2_client],

%      lists:map(fun(ClientId) ->
%        ExpectedReply = #{
%          <<"type">> => <<"RadarScanNotification">>,
%          <<"data">> => #{
%              <<"elements">> => [],
%              <<"walls">> => []
%             }
%        },

%        validate_last_reply_test(ClientId, ExpectedReply)
%      end, CliendIds)
%    end
%   }).

player_can_not_set_radar_type_before_game_starts_test() ->
  run_test(#{
    steps => fun(_Context) ->
      [
       register_player(ws_player_1_client),
       ws_client_sel_recv(ws_player_1_client, <<"RegisterPlayerAck">>),
       ws_client_send(ws_player_1_client, <<"{\"type\":\"ConfigurePlayerCommand\", \"data\":{\"op\":\"radarType\", \"args\": [\"longRange\"]}}">>),
       ws_client_recv(ws_player_1_client)
      ]
    end,

    test =>  validate_last_reply_type_test(ws_player_1_client, <<"InvalidCommandError">>)
   }).


%long_range_radar_detects_player_test() ->
%  run_test(#{
%    steps => fun(_Context) ->
%      [
%       register_player(ws_player_1_client),
%       register_player(ws_player_2_client),
%       ws_client_sel_recv(ws_player_1_client, <<"RegisterPlayerAck">>),
%       ws_client_sel_recv(ws_player_2_client, <<"RegisterPlayerAck">>),
%       fun(Context) ->
%           ScanningPlayer = get_player_for_client(ws_player_1_client, Context),
%           ScannedPlayer  = get_player_for_client(ws_player_2_client, Context),

%           % center the player to avoid the walls
%           pewpew_player_component:set_coordinates(ScanningPlayer, [{x, 200}, {y, 200}]),
%           pewpew_player_component:set_coordinates(ScannedPlayer, [{x, 250}, {y, 250}]),

%           {context, Context}
%       end,
%       ws_client_send(ws_control_client, #{type => <<"StartGameCommand">>, data => #{}}),
%       ws_client_sel_recv(ws_player_1_client, <<"StartGameOrder">>),
%       ws_client_send(ws_player_1_client, <<"{\"type\":\"ConfigurePlayerCommand\", \"data\":{\"op\":\"radarType\", \"args\": [\"longRange\"]}}">>),
%       ws_client_sel_recv(ws_player_1_client, <<"ConfigurePlayerAck">>),
%       ws_client_recv(ws_player_1_client),
%       ws_client_recv(ws_player_2_client)
%      ]
%    end,

%    test => fun(_) ->
%      CliendIds = [ ws_player_1_client, ws_player_2_client],

%      lists:map(fun(ClientId) ->
%        ExpectedReply = #{
%          <<"type">> => <<"RadarScanNotification">>,
%          <<"data">> => #{
%              <<"elements">> => [],
%              <<"walls">> => []
%             }
%        },

%        validate_last_reply_test(ClientId, ExpectedReply)
%      end, CliendIds)
%    end
%   }).
% RADAR TEST HITS WALLS
