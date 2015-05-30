-module(pewpew_tests).
-include_lib("eunit/include/eunit.hrl").

-import(pewpew_test_support, [
  run_test/1,
  ws_client_send/2,
  ws_client_recv/1,
  generate_reject_move_command_test/1,
  generate_valid_move_command_test/1
]).

%register_player_command_test_() ->
%  run_test(#{
%    steps => [
%      ws_client_send(ws_player_client, <<"{\"type\":\"RegisterPlayerCommand\", \"data\":{}}">>)
%    ],

%    test => fun(Context) ->
%      #{last_reply := JSON} = Context,

%      #{<<"type">> := AckType, <<"data">> := Data} = JSON,
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
%      ws_client_send(ws_player_client, <<"{\"type\":\"RegisterPlayerCommand\", \"data\":{}}">>),
%      ws_client_send(ws_player_client, <<"{\"type\":\"RegisterPlayerCommand\", \"data\":{}}">>)
%    ],

%    test => fun(Context) ->
%      #{last_reply := #{<<"type">> := Type}} = Context,

%      ?_assertEqual(<<"InvalidCommandError">>, Type)
%    end
% }).

%start_game_command_test_() ->
%  run_test(#{
%    steps => [
%      ws_client_send(ws_control_client, <<"{\"type\":\"StartGameCommand\", \"data\":{}}">>)
%    ],

%    test => fun(Context) ->
%      #{last_reply := #{<<"type">> := AckType}} = Context,

%      ?_assertEqual(<<"StartGameAck">>, AckType)
%    end
% }).

%reject_start_game_command_when_invalid_origin_test_() ->
%  run_test(#{
%    steps => [
%      ws_client_send(ws_player_client, <<"{\"type\":\"StartGameCommand\", \"data\":{}}">>)
%    ],

%    test => fun(Context) ->
%      #{last_reply := #{<<"type">> := Type}} = Context,

%      ?_assertEqual(<<"InvalidCommandError">>, Type)
%    end
%   }).

%reject_start_game_command_when_already_started_test_() ->
%  run_test(#{
%    steps => [
%      ws_client_send(ws_control_client, <<"{\"type\":\"StartGameCommand\", \"data\":{}}">>),
%      ws_client_send(ws_control_client, <<"{\"type\":\"StartGameCommand\", \"data\":{}}">>)
%    ],

%    test => fun(Context) ->
%      #{last_reply := #{<<"type">> := Type}} = Context,

%      ?_assertEqual(<<"InvalidCommandError">>, Type)
%    end
%   }).

%ws_client_send_start_game_order_to_players_test_() ->
%  run_test(#{
%    steps => [
%      ws_client_send(ws_player_client, <<"{\"type\":\"RegisterPlayerCommand\", \"data\":{}}">>),
%      ws_client_send(ws_control_client, <<"{\"type\":\"StartGameCommand\", \"data\":{}}">>),
%      ws_client_recv(ws_player_client)
%    ],

%    test => fun(Context) ->
%      #{last_reply := JSON} = Context,
%      #{<<"type">> := OrderType} = JSON,

%      ?_assertEqual(<<"StartGameOrder">>, OrderType)
%    end
%   }).



%reject_move_player_command_when_game_not_started_test_() ->
%  run_test(#{
%    steps => [
%        ws_client_send(ws_player_client, <<"{\"type\":\"RegisterPlayerCommand\", \"data\":{}}">>),
%        ws_client_send(ws_player_client, <<"{\"type\":\"MovePlayerCommand\", \"data\":[]}">>)
%      ],

%    test => fun(Context) ->
%      #{last_reply := JSON} = Context,
%      #{<<"type">> := OrderType} = JSON,

%      ?_assertEqual(<<"InvalidCommandError">>, OrderType)
%    end
%   }).

%reject_move_player_command_when_the_player_is_not_registered_test_() ->
%  run_test(#{
%    steps => [
%        ws_client_send(ws_control_client, <<"{\"type\":\"StartGameCommand\", \"data\":{}}">>),
%        ws_client_send(ws_player_client, <<"{\"type\":\"MovePlayerCommand\", \"data\":[]}">>)
%      ],

%    test => fun(Context) ->
%      #{last_reply := JSON} = Context,
%      #{<<"type">> := OrderType} = JSON,

%      ?_assertEqual(<<"InvalidCommandError">>, OrderType)
%    end
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

send_state_to_control_test_() ->
  run_test(#{
    steps => [ws_client_recv(ws_control_client)],
    test => fun(Context) ->
      #{last_reply := JSON} = Context,
      #{<<"arena">> := #{ <<"players">> := Players }} = JSON,
      ?_assertEqual([], Players)
    end
   }).

state_update_includes_registered_player_test_() ->
  run_test(#{
    steps => [
      ws_client_send(ws_player_client, #{type => <<"RegisterPlayerCommand">>, data => #{}}),
      ws_client_recv(ws_control_client)
    ],
    test => fun(Context) ->
      #{ pewpew_game := PewPewGame } = Context,
      Arena = pewpew_game:arena_component(PewPewGame),
      [Player] = pewpew_arena_component:players(Arena),
      ExpectedPlayerState = #{
        <<"coordinates">> => #{ <<"x">> => pewpew_player_component:x(Player), <<"y">> => pewpew_player_component:y(Player) }, <<"life">> => pewpew_player_component:life(Player), <<"rotation">> => pewpew_player_component:rotation(Player)
       },

      #{last_reply := JSON} = Context,
      #{<<"arena">> := #{ <<"players">> := Players }} = JSON,

      PlayersState = lists:nth(1, Players),
      [
       ?_assertEqual(1, length(Players)),
       ?_assertEqual(ExpectedPlayerState, PlayersState)
      ]
    end
   }).
