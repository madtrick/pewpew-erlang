-module(pewpew_test_support).
-include_lib("eunit/include/eunit.hrl").

-export([
  run_test/1,
  ws_client_send/2,
  ws_client_recv/1,
  ws_client_flush/1,
  ws_client_count_recv/2,
  ws_client_sel_recv/2,
  generate_reject_move_command_test/1,
  generate_valid_move_command_test/1,
  register_player/0,
  wait/1,
  get_player_for_client/2,
  get_last_reply_for_client/2,
  validate_last_reply_type_test/2
]).

run_test(Config) ->
  {setup,
    fun() ->
      meck:new(pewpew_core, [passthrough]),
      application:set_env(pewpew, execution_mode, test),
      pewpew:start(),
      {ok, Client}        = ws_client:start_link(),
      {ok, ControlClient} = ws_client:start_link(4321),
      [PewPewGame]        = pewpew_core:get_games(),
      ArenaComponent      = pewpew_game:arena_component(PewPewGame),

      #{
        ws_player_client => Client,
        ws_control_client => ControlClient,
        pewpew_game => PewPewGame,
        arena_component => ArenaComponent,
        replies => [],
        per_client_replies => #{},
        last_reply_per_client => #{},
        players => #{} % TODO: remove this element
      }
    end,
    fun(Context) ->
      #{
        ws_player_client := Client,
        ws_control_client := ControlClient
      } = Context,

      ws_client:stop(Client),
      ws_client:stop(ControlClient),

      pewpew:stop(),
      meck:unload(pewpew_core)
    end,
    fun(Context) ->
      % TODO: maybe remove this default for the test function
      Test   = maps:get(test, Config, fun(_) -> [] end),
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
             run_tests(Test, NewContext)
          end;
        _ ->
          case Steps of
            undefined ->
              UpdatedContext = Before(Context),
              Test(UpdatedContext);
            _ ->
              UpdatedContext = Before(Context),
              NewContext     = execute_steps(Steps(UpdatedContext), UpdatedContext),
              run_tests(Test, NewContext)
          end
      end
    end
    }.

run_list_of_tests([], _, TestObjects) ->
  TestObjects;
run_list_of_tests([{_, _} = T | Tail], Context, TestObjects) ->
  run_list_of_tests(Tail, Context, [T | TestObjects]);
run_list_of_tests([T | Tail], Context, TestObjects) ->
  NewTestObjects      = T(Context),
  UpdatedTestsObjects = lists:flatten([NewTestObjects | TestObjects]),
  run_list_of_tests(Tail, Context, UpdatedTestsObjects).

run_tests(Tests, Context) when is_function(Tests) ->
  Tests(Context);
run_tests(Tests, Context) when is_list(Tests) ->
  run_list_of_tests(Tests, Context, []).

%register_player() ->
%  register_player(last_registered_player).
register_player() ->
  [
    ws_client_send(ws_player_client, #{type => <<"RegisterPlayerCommand">>, data => #{}})
    %fun(Context) ->
    %  #{
    %  arena_component := ArenaComponent,
    %  players := CurrentPlayers,
    %  last_reply := LastReply,
    %  per_client_replies := PerClientReplies
    %  } = Context,

    %  ?debugVal(LastReply),

    %  [#{<<"data">> := #{<<"id">> := PlayerId}}] = LastReply,
    %  Player = pewpew_arena_component:get_player(ArenaComponent, PlayerId),

    %  RepliesForClient        = maps:get(ws_player_client, PerClientReplies, []),
    %  % encode again as JSON as the 'execute_steps' function expect a JSON string
    %  UpdatedRepliesForClient = [jiffy:encode(LastReply) | RepliesForClient],
    %  UpdatedPerClientReplies = maps:put(ws_player_client, UpdatedRepliesForClient, PerClientReplies),

    %  UpdatedContext = Context#{
    %    players => maps:put(Alias, Player, CurrentPlayers),
    %    per_client_replies => UpdatedPerClientReplies
    %  },

    %  {context, UpdatedContext}
    %end
  ].

get_player_for_client(ClientId, Context) ->
  #{
    arena_component := ArenaComponent,
    per_client_replies := PerClientReplies
  } = Context,

  Replies = lists:flatten(maps:get(ClientId, PerClientReplies)),

  [RegisterPlayerAck] = [Reply || #{<<"type">> := Type} = Reply <- Replies, Type =:= <<"RegisterPlayerAck">>],

  #{<<"data">> := #{<<"id">> := PlayerId}} = RegisterPlayerAck,

  pewpew_arena_component:get_player(ArenaComponent, PlayerId).


% TODO: rename this function
lol(0, ClientId, _, Replies) ->
  {replies, ClientId, lists:reverse(Replies)};
lol(Counter, ClientId, Client, Replies) ->
  {text, Reply} = ws_client:recv(Client),
  lol(Counter - 1, ClientId, Client,  [Reply | Replies]).

ws_client_recv(ClientId) ->
  ws_client_count_recv(ClientId, 1).

ws_client_count_recv(ClientId, Count) ->
  fun(Context) ->
    Client = maps:get(ClientId, Context),
    % TODO why might not need to get the client replies
    %PerClientReplies = maps:get(per_client_replies, Context),
    %ClientReplies = maps:get(ClientId, PerClientReplies, []),
    lol(Count, ClientId, Client, [])
    %F = fun (0, Replies) ->
    %          {replies, ClientId, Replies};
    %        (Counter, Replies) ->
    %          Client = maps:get(ClientId, Context),
    %          {text, Reply} = ws_client:recv(Client),
    %          F(Counter - 1, [Reply | Replies])
    %end,

    %F(Count, [])
  end.

ws_client_send(ClientId, Message) ->
  JSON = maybe_convert_message_to_json(Message),

  fun(Context) ->
      Client = maps:get(ClientId, Context),

      ws_client:send_text(Client, JSON)
  end.

ws_client_sel_recv(ClientId, Client, Type, Replies) ->
  {text, Reply} = ws_client:recv(Client),
  JSON = jiffy:decode(Reply, [return_maps]),

  [#{<<"type">> := ReplyType}] = JSON,
  case ReplyType of
    Type ->  {replies, ClientId, lists:reverse([Reply | Replies])};
    _ -> ws_client_sel_recv(ClientId, Client, Type, [Reply | Replies])
  end.

ws_client_sel_recv(ClientId, Type) ->
  fun(Context) ->
      Client = maps:get(ClientId, Context),

      ws_client_sel_recv(ClientId, Client, Type, [])
  end.

ws_client_flush(ClientId) ->
  fun(Context) ->
      Client = maps:get(ClientId, Context),
      ws_client:flush(Client),
      ok
  end.

% TODO maybe remove this function
wait(Time) ->
  fun(_) ->
    timer:sleep(Time)
  end.

get_last_reply_for_client(ClientId, Context) ->
  #{per_client_replies := PerClientReplies} = Context,

  Replies = maps:get(ClientId, PerClientReplies),
  Last    = lists:last(Replies),
  Last.

generate_reject_move_command_test(Options) ->
  DefaultOptions = #{
    move_player_reply => <<"InvalidCommandError">>,
    test => [
             validate_last_reply_type_test(ws_player_client, <<"InvalidCommandError">>),
             fun(Context) ->
               #{
                 coordinates_before_move := CoordinatesBeforeMove,
                 coordinates_after_move := CoordinatesAfterMove
                } = Context,

               [
                ?_assertEqual(CoordinatesBeforeMove, CoordinatesAfterMove)
               ]
             end
            ]
    %test => fun(Context) ->
    %  #{
    %    last_reply_per_client := #{ws_player_client := JSON},
    %    coordinates_before_move := CoordinatesBeforeMove,
    %    coordinates_after_move := CoordinatesAfterMove
    %  } = Context,

    %  [
    %   #{<<"type">> := OrderType}
    %  ] = JSON,

    %  [
    %   ?_assertEqual(<<"InvalidCommandError">>, OrderType),
    %   ?_assertEqual(CoordinatesBeforeMove, CoordinatesAfterMove)
    %  ]
    %end
  },

  GenerateMoveCommandOptions = maps:merge(DefaultOptions, Options),
  generate_move_command(GenerateMoveCommandOptions).

generate_valid_move_command_test(Options) ->
  #{
    expectations := #{
      x := ExpectedX,
      y := ExpectedY
     }
  } = Options,

  generate_move_command(
    maps:merge(Options,
      #{
        move_player_reply => <<"MovePlayerAck">>,
        test => fun(Context) ->
          JSON = get_last_reply_for_client(ws_player_client, Context),

          [
           #{<<"type">> := Type, <<"data">> := Data}
          ] = JSON,

          [
           ?_assertEqual(<<"MovePlayerAck">>, Type),
           ?_assertEqual(#{<<"x">> => ExpectedX, <<"y">> => ExpectedY}, Data)
          ]
        end
     }
    )
 ).

validate_last_reply_type_test(ClientId, ExpectedType) ->
  fun(Context) ->
    Reply = get_last_reply_for_client(ClientId, Context),

    [
     #{<<"type">> := Type}
    ] = Reply,

    ?_assertEqual(ExpectedType, Type)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

execute_steps([], Context) ->
  #{replies := Replies} = Context,

  OrderedReplies  = lists:reverse(Replies),

  % TODO maybe remove the Replies property
  Context#{replies => OrderedReplies};
execute_steps([Step | Tail], Context) ->
  UpdatedContext = execute_step(Step, Context),
  execute_steps(Tail, UpdatedContext).

execute_step(NestedSteps, Context) when is_list(NestedSteps) ->
  lists:foldl(fun(Step, Acc) ->
    execute_step(Step, Acc)
  end, Context, NestedSteps);
execute_step(Fun, Context) ->
  Result = Fun(Context),

  case Result of
    {reply, Reply} -> % TODO remove this case
      JSON = jiffy:decode(Reply, [return_maps]),

      #{replies := CurrentReplies} = Context,
      Context#{
        replies => [JSON | CurrentReplies],
        last_reply => JSON
      };
    {replies, ClientId, Replies} ->
      #{
        per_client_replies := PerClientReplies,
        last_reply_per_client := LastReplyPerClient
      } = Context,

      CurrentReplies = maps:get(ClientId, PerClientReplies, []),

      JSON      = lists:map(fun(E) -> jiffy:decode(E, [return_maps]) end, Replies),
      LastReply = lists:last(JSON),

      UpdatedPerClientReplies  = maps:put(ClientId, CurrentReplies ++ JSON, PerClientReplies),
      UpdatedLasReplyPerClient = maps:put(ClientId, LastReply, LastReplyPerClient),

      Context#{
        per_client_replies => UpdatedPerClientReplies,
        last_reply_per_client => UpdatedLasReplyPerClient
       };
    {context, UpdatedContext} ->
      UpdatedContext;
    _ -> Context
  end.

generate_move_command(Options) ->
  #{
    move_player_reply := MovePlayerReply,
    test := Test
   } = Options,

  DefaultCoordinates   = [{x, 10}, {y, 10}],
  CoordinatesListOrFun = maps:get(coordinates, Options, DefaultCoordinates),
  Movements            = maps:get(movements, Options, <<"[]">>),
  JSONifiedMovements   = jiffy:encode(Movements),

  run_test(#{
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
      register_player(),
      ws_client_sel_recv(ws_player_client, <<"RegisterPlayerAck">>),
      SetPlayerCoordinates,
      ws_client_send(ws_control_client, <<"{\"type\":\"StartGameCommand\", \"data\":{}}">>),
      ws_client_sel_recv(ws_player_client, <<"StartGameOrder">>),
      GetPlayerCoordinates(coordinates_before_move),
      ws_client_send(ws_player_client, <<"{\"type\":\"MovePlayerCommand\", \"data\":", JSONifiedMovements/binary, "}">>),
      ws_client_sel_recv(ws_player_client, MovePlayerReply),
      GetPlayerCoordinates(coordinates_after_move)
    ]
    end,

    test => Test
   }).

maybe_convert_message_to_json(Message) when is_map(Message) ->
  jiffy:encode(Message);
maybe_convert_message_to_json(Message) ->
  Message.
