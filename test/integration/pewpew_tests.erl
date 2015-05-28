-module(pewpew_tests).
-include_lib("eunit/include/eunit.hrl").

execute_steps([], Context) ->
  #{replies := Replies} = Context,

  [LastReply | _] = Replies,
  OrderedReplies  = lists:reverse(Replies),

  Context#{
    replies => OrderedReplies,
    last_reply => LastReply
   };
execute_steps([Step | Tail], Context) ->
  UpdatedContext = execute_step(Step, Context),
  execute_steps(Tail, UpdatedContext).

execute_step(Fun, Context) ->
  Result = Fun(Context),

  case Result of
    {reply, Reply} ->
      JSON = jiffy:decode(Reply, [return_maps]),

      #{replies := CurrentReplies} = Context,
      Context#{replies => [JSON | CurrentReplies]};
    {context, UpdatedContext} ->
      UpdatedContext;
    _ -> Context
  end.


run(Config) ->
  {setup,
    fun() ->
      application:set_env(pewpew, execution_mode, test),
      pewpew:start(),
      {ok, Client}        = ws_client:start_link(),
      {ok, ControlClient} = ws_client:start_link(4321),
      [PewPewGame]        = pewpew_core:get_games(),

      #{
        ws_player_client => Client,
        ws_control_client => ControlClient,
        pewpew_game => PewPewGame,
        replies => []
      }
    end,
    fun(Context) ->
      #{
        ws_player_client := Client,
        ws_control_client := ControlClient
      } = Context,

      ws_client:stop(Client),
      ws_client:stop(ControlClient),

      pewpew:stop()
    end,
    fun(Context) ->
      Test   = maps:get(test, Config),
      Before = maps:get(before, Config, undefined),
      Steps  = maps:get(steps, Config, undefined),

      case Before of
        undefined ->
          case Steps of
            undefined ->
              Test(Context);
            _ ->

             StepsList = case is_function(Steps) of
               true -> Steps(Context);
               false -> Steps
             end,

             NewContext = execute_steps(StepsList, Context),
             Test(NewContext)
          end;
        _ ->
          case Steps of
            undefined ->
              UpdatedContext = Before(Context),
              Test(UpdatedContext);
            _ ->
              UpdatedContext = Before(Context),
             NewContext      = execute_steps(Steps(UpdatedContext), UpdatedContext),
             Test(NewContext)
          end
      end
    end
    }.

register_player_command_test_() ->
  run(#{
    steps => [
      send(ws_player_client, <<"{\"type\":\"RegisterPlayerCommand\", \"data\":{}}">>)
    ],

    test => fun(Context) ->
      #{last_reply := JSON} = Context,

      #{<<"type">> := AckType, <<"data">> := Data} = JSON,
      #{<<"id">> := Id, <<"x">> := X, <<"y">> := Y, <<"life">> := Life} = Data,

      [
        ?_assertEqual(<<"RegisterPlayerAck">>, AckType),
        ?_assert(is_integer(Id)),
        ?_assert(is_integer(X)),
        ?_assert(is_integer(Y)),
        ?_assert(is_integer(Life))
      ]
    end
   }).

reject_register_player_twice_test_() ->
  run(#{
    steps => [
      send(ws_player_client, <<"{\"type\":\"RegisterPlayerCommand\", \"data\":{}}">>),
      send(ws_player_client, <<"{\"type\":\"RegisterPlayerCommand\", \"data\":{}}">>)
    ],

    test => fun(Context) ->
      #{last_reply := #{<<"type">> := Type}} = Context,

      ?_assertEqual(<<"InvalidCommandError">>, Type)
    end
 }).

start_game_command_test_() ->
  run(#{
    steps => [
      send(ws_control_client, <<"{\"type\":\"StartGameCommand\", \"data\":{}}">>)
    ],

    test => fun(Context) ->
      #{last_reply := #{<<"type">> := AckType}} = Context,

      ?_assertEqual(<<"StartGameAck">>, AckType)
    end
 }).

reject_start_game_command_when_invalid_origin_test_() ->
  run(#{
    steps => [
      send(ws_player_client, <<"{\"type\":\"StartGameCommand\", \"data\":{}}">>)
    ],

    test => fun(Context) ->
      #{last_reply := #{<<"type">> := Type}} = Context,

      ?_assertEqual(<<"InvalidCommandError">>, Type)
    end
   }).

reject_start_game_command_when_already_started_test_() ->
  run(#{
    steps => [
      send(ws_control_client, <<"{\"type\":\"StartGameCommand\", \"data\":{}}">>),
      send(ws_control_client, <<"{\"type\":\"StartGameCommand\", \"data\":{}}">>)
    ],

    test => fun(Context) ->
      #{last_reply := #{<<"type">> := Type}} = Context,

      ?_assertEqual(<<"InvalidCommandError">>, Type)
    end
   }).

send_start_game_order_to_players_test_() ->
  run(#{
    steps => [
      send(ws_player_client, <<"{\"type\":\"RegisterPlayerCommand\", \"data\":{}}">>),
      send(ws_control_client, <<"{\"type\":\"StartGameCommand\", \"data\":{}}">>),
      recv(ws_player_client)
    ],

    test => fun(Context) ->
      #{last_reply := JSON} = Context,
      #{<<"type">> := OrderType} = JSON,

      ?_assertEqual(<<"StartGameOrder">>, OrderType)
    end
   }).

recv(ClientId) ->
  fun(Context) ->
    Client = maps:get(ClientId, Context),
    {text, Reply} = ws_client:recv(Client),
    {reply, Reply}
  end.

send(ClientId, Message) ->
  fun(Context) ->
      Client = maps:get(ClientId, Context),

      ws_client:send_text(Client, Message),
      {text, Reply} = ws_client:recv(Client),
      {reply, Reply}
  end.

reject_move_player_command_when_game_not_started_test_() ->
  run(#{
    steps => [
        send(ws_player_client, <<"{\"type\":\"RegisterPlayerCommand\", \"data\":{}}">>),
        send(ws_player_client, <<"{\"type\":\"MovePlayerCommand\", \"data\":[]}">>)
      ],

    test => fun(Context) ->
      #{last_reply := JSON} = Context,
      #{<<"type">> := OrderType} = JSON,

      ?_assertEqual(<<"InvalidCommandError">>, OrderType)
    end
   }).

reject_move_player_command_when_the_player_is_not_registered_test_() ->
  run(#{
    steps => [
        send(ws_control_client, <<"{\"type\":\"StartGameCommand\", \"data\":{}}">>),
        send(ws_player_client, <<"{\"type\":\"MovePlayerCommand\", \"data\":[]}">>)
      ],

    test => fun(Context) ->
      #{last_reply := JSON} = Context,
      #{<<"type">> := OrderType} = JSON,

      ?_assertEqual(<<"InvalidCommandError">>, OrderType)
    end
   }).

generate_move_command(Options) ->
  #{
    test := Test
   } = Options,

  DefaultCoordinates   = [{x, 10}, {y, 10}],
  CoordinatesListOrFun = maps:get(coordinates, Options, DefaultCoordinates),
  Movements            = maps:get(movements, Options, <<"[]">>),
  JSONifiedMovements   = jiffy:encode(Movements),

  run(#{
    steps => fun(Context) ->
      #{ pewpew_game := Game } = Context,
      ArenaComponent                 = pewpew_game:arena_component(Game),
      {width, Width, height, Height} = pewpew_arena_component:dimensions(ArenaComponent),

      SetPlayerCoordinates = fun(_) ->
        [Player]    = pewpew_arena_component:players(ArenaComponent),
        Coordinates = case erlang:is_function(CoordinatesListOrFun) of
                        true -> CoordinatesListOrFun(Width, Height);
                        false -> CoordinatesListOrFun
                      end,
        pewpew_player_component:set_coordinates(Player, Coordinates)
      end,

      GetPlayerCoordinates = fun(Tag) ->
        fun(Context_2) ->
          [Player]       = pewpew_arena_component:players(ArenaComponent),
          Coordinates    = pewpew_player_component:coordinates(Player),
          UpdatedContext = maps:put(Tag, Coordinates, Context_2),
          {context, UpdatedContext}
        end
      end,

    [
      send(ws_player_client, <<"{\"type\":\"RegisterPlayerCommand\", \"data\":{}}">>),
      SetPlayerCoordinates,
      send(ws_control_client, <<"{\"type\":\"StartGameCommand\", \"data\":{}}">>),
      GetPlayerCoordinates(coordinates_before_move),
      recv(ws_player_client),
      send(ws_player_client, <<"{\"type\":\"MovePlayerCommand\", \"data\":", JSONifiedMovements/binary, "}">>),
      GetPlayerCoordinates(coordinates_after_move)
    ]
    end,

    test => Test
   }).

generate_reject_move_command_test(Options) ->
  DefaultOptions = #{
    test => fun(Context) ->
      #{
        last_reply := JSON,
        coordinates_before_move := CoordinatesBeforeMove,
        coordinates_after_move := CoordinatesAfterMove
      } = Context,

      #{<<"type">> := OrderType} = JSON,

      [
       ?_assertEqual(<<"InvalidCommandError">>, OrderType),
       ?_assertEqual(CoordinatesBeforeMove, CoordinatesAfterMove)
      ]
    end
  },

  GenerateMoveCommandOptions = maps:merge(DefaultOptions, Options),
  generate_move_command(GenerateMoveCommandOptions).

reject_move_player_command_when_player_hits_arena_edges_test_() ->
  generate_reject_move_command_test(#{
    coordinates => fun(ArenaWidth, _) -> [{x, ArenaWidth}] end,
    movements => [#{move => forward}]
   }).

reject_move_player_command_when_player_hits_arena_edges_include_radius_test_() ->
  generate_reject_move_command_test(#{
    coordinates => fun(ArenaWidth, _) -> [{x, ArenaWidth - 2}] end,
    movements => [#{move => forward}]
   }).

reject_move_player_command_when_player_hits_negative_arena_edges_test_() ->
  generate_reject_move_command_test(#{
    coordinates => [{x, 6}, {y, 6}],
    movements => [#{move => backward}]
   }).

reject_move_player_command_when_direction_is_invalid_test_() ->
  generate_reject_move_command_test(#{
    movements => [#{move => invalid_movement}]
   }).

reject_move_player_command_when_rotation_is_invalid_test_() ->
  generate_reject_move_command_test(#{
    movements => [#{rotate => 900}]
   }).

reject_move_player_command_when_two_rotations_test_() ->
  generate_reject_move_command_test(#{
    movements => [#{rotate => 2}, #{rotate => 2}]
   }).

reject_move_player_command_when_two_moves_test_() ->
  generate_reject_move_command_test(#{
    movements => [#{move => forward}, #{move => forward}]
   }).

generate_movement_test(Options) ->
  #{
    expectations := #{
      x := ExpectedX,
      y := ExpectedY
     }
  } = Options,

  generate_move_command(
    maps:merge(Options,
      #{
        test => fun(Context) ->
          #{last_reply := #{<<"type">> := Type, <<"data">> := Data}} = Context,

          [
           ?_assertEqual(<<"MovePlayerAck">>, Type),
           ?_assertEqual(#{<<"x">> => ExpectedX, <<"y">> => ExpectedY}, Data)
          ]
        end
     }
    )
 ).


move_player_test_() ->
  Coordinates = fun(_, _) -> [{x, 10}, {y, 10}] end,
  Movements = [
   #{
    coordinates => Coordinates,
    movements => [#{rotate => 60}, #{move => forward}],
    expectations => #{x => 10.5, y =>10.86602540378444}
    },
   #{
    coordinates => Coordinates,
    movements => [#{rotate => 60}],
    expectations => #{x => 10, y =>10}
    }
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

  lists:map(fun generate_movement_test/1, Movements).
