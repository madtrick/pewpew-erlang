-include_lib("eunit/include/eunit.hrl").
-module(pewpew_test_support).

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
  register_player/1,
  get_player_for_client/2,
  get_nth_reply_for_client/3,
  get_replies_for_client/2,
  get_last_reply_for_client/2,
  get_message_in_last_reply_for_client/3,
  validate_type_in_last_reply_test/2,
  validate_last_reply_data_test/2,
  validate_last_reply_data_for_type_test/3,
  validate_message_in_nth_reply_test/3,
  validate_message_in_last_reply_test/2,
  validate_message_in_last_reply_matches/2,
  validate_message_matches/2,
  throwing/1,
  it_threw/1,
  place_player_at/2,
  test_step/1,
  is_ws_client_alive/2,
  is_ws_client_dead/2
]).

run_test(Config) ->
  {setup,
    fun() ->
      application:set_env(pewpew, execution_mode, test),
      pewpew:start(),
      {ok, ControlClient} = ws_client:start_link(4321),
      [PewPewGame]        = pewpew_core:get_games(),
      ArenaComponent      = pewpew_game:arena_component(PewPewGame),

      #{
        clients               => #{ws_control_client => ControlClient},
        pewpew_game           => PewPewGame,
        arena_component       => ArenaComponent,
        per_client_replies    => #{},
        last_thrown_exception => undefined,
        tests                 => []
      }
    end,
    fun(Context) ->
      #{
        clients := Clients
      } = Context,

      ClientsPids = maps:values(Clients),
      [ws_client:stop(ClientPid) || ClientPid <- ClientsPids],

      pewpew:stop()
    end,
    fun(Context) ->
      Test   = maps:get(test, Config, []),
      Steps  = maps:get(steps, Config, undefined),

      StepsList = case is_function(Steps) of
                    true -> Steps(Context);
                    false -> Steps
                  end,

      NewContext = execute_steps(StepsList, Context),
      #{tests := ContextTests} = NewContext,
      Tests = lists:flatten(lists:reverse([Test | ContextTests])),
      run_tests(Tests, NewContext)
    end
  }.

run_list_of_tests([], _, TestObjects) ->
  TestObjects;
run_list_of_tests([{_, _} = T | Tail], Context, TestObjects) ->
  run_list_of_tests(Tail, Context, [T | TestObjects]);
run_list_of_tests([T | Tail], Context, TestObjects) ->
  NewTestObjects      = run_tests(T, Context),
  UpdatedTestsObjects = lists:flatten([NewTestObjects | TestObjects]),
  run_list_of_tests(Tail, Context, UpdatedTestsObjects).

run_tests({_, _} = Test, _) ->
  Test;
run_tests(Tests, Context) when is_function(Tests) ->
  Result = Tests(Context),
  case Result of
    %TODO: handle {test, _} Results
    Fun when is_function(Fun) -> Fun(Context); % TODO: do we really need this case?
    List when is_list(List) -> run_list_of_tests(List, Context, []);
    _ -> Result
  end;
run_tests(Tests, Context) when is_list(Tests) ->
  run_list_of_tests(Tests, Context, []).

register_player() ->
  register_player(ws_player_client).
register_player(ClientId) ->
  ws_client_send(ClientId, #{type => <<"RegisterPlayerCommand">>, data => #{}}).

get_player_for_client(ClientId, Context) ->
  #{
    arena_component := ArenaComponent,
    per_client_replies := PerClientReplies
  } = Context,

  Replies = lists:flatten(maps:get(ClientId, PerClientReplies)),

  [RegisterPlayerAck] = [Reply || #{<<"type">> := Type} = Reply <- Replies, Type =:= <<"RegisterPlayerAck">>],

  #{<<"data">> := #{<<"id">> := PlayerId}} = RegisterPlayerAck,

  pewpew_arena_component:get_player(ArenaComponent, PlayerId).


ws_client_recv(ClientId) ->
  ws_client_count_recv(ClientId, 1).

ws_client_count_recv(0, ClientId, _, Replies) ->
  {replies, ClientId, lists:reverse(Replies)};
ws_client_count_recv(Counter, ClientId, Client, Replies) ->
  {text, Reply} = ws_client:recv(Client),
  ws_client_count_recv(Counter - 1, ClientId, Client,  [Reply | Replies]).

ws_client_count_recv(ClientId, Count) ->
  fun(Context) ->
    Client = get_client(ClientId, Context),
    ws_client_count_recv(Count, ClientId, Client, [])
  end.

ws_client_send(ClientId, Message) ->
  JSON = maybe_convert_message_to_json(Message),

  fun(Context) ->
      {Client, UpdatedContext} = maybe_start_client(ClientId, Context),

      ws_client:send_text(Client, JSON),

      {context, UpdatedContext}
  end.

ws_client_sel_recv(ClientId, Client, Type, Timeout, Replies) ->
  Now = pewpew_utils:get_current_time_in_milliseconds(),

  case Now >= Timeout of
    true ->
      ?debugMsg("Timeout waiting for " ++ Type),
      (throw(ws_client_sel_recv_timeout));
    false ->
      {text, Reply} = ws_client:recv(Client),
      JSON = jiffy:decode(Reply, [return_maps]),

      Any = lists:any(fun(#{<<"type">> := ReplyType}) ->
                          ReplyType =:= Type
      end, JSON),

      case Any of
        true -> 
          {replies, ClientId, lists:reverse([Reply | Replies])};
        _ -> ws_client_sel_recv(ClientId, Client, Type, Timeout, [Reply | Replies])
      end
  end.

ws_client_sel_recv(ClientId, Type) ->
  fun(Context) ->
      Now = pewpew_utils:get_current_time_in_milliseconds(),
      Timeout = 1 * 1000,
      Client = get_client(ClientId, Context),

      ws_client_sel_recv(ClientId, Client, Type, Now + Timeout, [])
  end.

ws_client_flush(ClientId) ->
  fun(Context) ->
      Client = get_client(ClientId, Context),
      ws_client:flush(Client),
      ok
  end.

get_nth_reply_for_client(Number, ClientId, Context) when Number >= 1 ->
  Replies = get_replies_for_client(ClientId, Context),
  lists:nth(Number, Replies);
get_nth_reply_for_client(0, _, _) ->
  throw(invalid_reply_index_0);
get_nth_reply_for_client(Number, ClientId, Context) ->
  Replies = get_replies_for_client(ClientId, Context),
  RepliesLength = length(Replies),
  lists:nth(RepliesLength + Number + 1, Replies).

get_replies_for_client(ClientId, Context) ->
  #{per_client_replies := PerClientReplies} = Context,
  maps:get(ClientId, PerClientReplies).

get_last_reply_for_client(ClientId, Context) ->
  Replies = get_replies_for_client(ClientId, Context),
  Last    = lists:last(Replies),
  Last.

get_message_in_last_reply_for_client(CliendId, MessageType, Context) ->
  LastReply = get_last_reply_for_client(CliendId, Context),

  [Message] = lists:filter(fun(Message) ->
          #{<<"type">> := Type} = Message,
          Type =:= MessageType
    end, LastReply),
  Message.

generate_reject_move_command_test(Options) ->
  DefaultOptions = #{
    move_player_reply => <<"InvalidCommandError">>,
    test => [
      validate_type_in_last_reply_test(ws_player_client, <<"InvalidCommandError">>),
      fun(Context) ->
       #{
         coordinates_before_move := CoordinatesBeforeMove,
         coordinates_after_move := CoordinatesAfterMove
        } = Context,

       [?_assertEqual(CoordinatesBeforeMove, CoordinatesAfterMove)]
      end
    ]
  },

  GenerateMoveCommandOptions = maps:merge(DefaultOptions, Options),
  generate_move_command(GenerateMoveCommandOptions).

generate_valid_move_command_test(Options) ->
  #{ coordinates := Coordinates, movements := Movements } = Options,

  InitialX = proplists:get_value(x, Coordinates),
  InitialY = proplists:get_value(y, Coordinates),

  generate_move_command(
    maps:merge(Options,
      #{
        move_player_reply => <<"MovePlayerAck">>,
        test => fun (_) ->
            Speed = pewpew_config:get([players, movement, speed]),

            Expectations = lists:foldl(
              fun(#{rotate := Rotation}, Acc) ->
                maps:put(rotation, Rotation, Acc);
              (#{move := forward}, Acc) ->
                #{rotation := R, coordinates := C} = Acc,
                UpdatedCoordinates = pewpew_utils:translate_point_by_vector(Speed, R, C),
                maps:put(coordinates, UpdatedCoordinates, Acc);
              (#{move := backward}, Acc) ->
                #{rotation := R, coordinates := C} = Acc,
                UpdatedCoordinates = pewpew_utils:translate_point_by_vector(Speed * -1, R, C),
                maps:put(coordinates, UpdatedCoordinates, Acc)

              end, #{rotation => 0, coordinates => {x, InitialX, y, InitialY}}, Movements),

            #{
              coordinates:= {x, ExpectedX, y, ExpectedY},
              rotation := ExpectedRotation
              } = Expectations,

            MessageExpectation = #{
                <<"type">> => <<"MovePlayerAck">>,
                <<"data">> => #{
                  <<"x">> => ExpectedX,
                  <<"y">> => ExpectedY,
                  <<"rotation">> => ExpectedRotation
                  }
                },

            validate_message_in_last_reply_matches(ws_player_client, MessageExpectation)
        end
     }
    )
 ).

validate_message_in_nth_reply_test(Number, ClientId, ExpectedMessage) ->
  Matcher = fun (Message, Expectation) -> Message =:= Expectation end,
  validate_message_in_nth_reply_test(Number, ClientId, ExpectedMessage, Matcher).

validate_message_in_nth_reply_test(Number, ClientId, ExpectedMessage, Matcher) ->
  fun(Context) ->
    Replies = get_nth_reply_for_client(Number, ClientId, Context),

    MessagePresent = lists:any(fun(Message) ->
              Matcher(Message, ExpectedMessage)
    end, Replies),

    not(MessagePresent) andalso ?debugVal(Replies),
    not(MessagePresent) andalso ?debugVal(ExpectedMessage),

    ?_assert(MessagePresent)
  end.

validate_message_in_last_reply_test(ClientId, ExpectedMessage) ->
  validate_message_in_nth_reply_test(-1, ClientId, ExpectedMessage).

validate_message_in_last_reply_matches(ClientId, ExpectedMessage) ->
  validate_message_in_nth_reply_test(-1, ClientId, ExpectedMessage, fun expectation_matches_message/2).

validate_message_matches(Message, ExpectedMessage) ->
  fun (_) ->
      Matches = expectation_matches_message(Message, ExpectedMessage),

      not Matches andalso ?debugVal(Message),
      not Matches andalso ?debugVal(ExpectedMessage),

      ?_assert(Matches)
  end.

validate_type_in_last_reply_test(ClientId, ExpectedType) ->
  fun(Context) ->
    Reply       = get_last_reply_for_client(ClientId, Context),
    TypePresent = is_reply_type_present_in_messages(ExpectedType, Reply),

    not TypePresent andalso ?debugVal(ExpectedType),

    ?_assert(TypePresent)
  end.

validate_last_reply_data_for_type_test(ClientId, ExpectedData, Type) ->
  fun(Context) ->
      Reply = get_last_reply_for_client(ClientId, Context),

      [ReplyOfType] = lists:filter(fun(R) ->
          #{<<"type">> := T} = R,
          T =:= Type
      end, Reply),

      #{<<"data">> := Data} = ReplyOfType,

      ?_assertEqual(ExpectedData, Data)
  end.

validate_last_reply_data_test(ClientId, ExpectedData) ->
  fun(Context) ->
    Reply = get_last_reply_for_client(ClientId, Context),

    [
     #{<<"data">> := Data}
    ] = Reply,

    ?_assertEqual(ExpectedData, Data)
  end.

throwing(Fun) ->
  fun(Context) ->
      try Fun(Context)
      catch
        Exception ->
          UpdatedContext = Context#{last_thrown_exception => Exception},
          {context, UpdatedContext}
      end
  end.

it_threw(Exception) ->
  fun (Context) ->
    #{last_thrown_exception := LastThrownException} = Context,

    ?_assertEqual(Exception, LastThrownException)
  end.

place_player_at(CliendId, Coordinates) ->
  fun (Context) ->
    Player = get_player_for_client(CliendId, Context),
    pewpew_player_component:set_coordinates(Player, Coordinates),
    ok
  end.

test_step(Step) when is_function(Step) ->
  fun (Context) ->
      {test, Step(Context)}
  end;
test_step(Step) ->
  fun (_) ->
      {test, Step}
  end.

is_ws_client_alive(ClientId, Context) ->
  #{clients := Clients} = Context,
  Client = maps:get(ClientId, Clients),
  is_process_alive(Client).

is_ws_client_dead(ClientId, Context) ->
  not is_ws_client_alive(ClientId, Context).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_reply_type_present_in_messages(Type, Messages) ->
  lists:any(fun(#{<<"type">> := ReplyType}) ->
    ReplyType =:= Type
  end, Messages).

execute_steps([], Context) ->
  Context;
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
    {replies, ClientId, Replies} ->
      #{ per_client_replies := PerClientReplies } = Context,

      CurrentReplies = maps:get(ClientId, PerClientReplies, []),

      JSON      = lists:map(fun(E) -> jiffy:decode(E, [return_maps]) end, Replies),

      UpdatedPerClientReplies  = maps:put(ClientId, CurrentReplies ++ JSON, PerClientReplies),

      Context#{ per_client_replies => UpdatedPerClientReplies };
    {context, UpdatedContext} ->
      UpdatedContext;
    {test, Test} ->
      #{tests := Tests} = Context,
      Context#{tests => [Test | Tests]};
    List when is_list(List) ->
      execute_steps(List, Context);
    NestedStep when is_function(NestedStep) ->
      execute_step(NestedStep, Context);
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

get_client(ClientId, Context) ->
  #{clients := Clients} =  Context,
  Client = maps:get(ClientId, Clients, undefined),

  Client.

maybe_start_client(ClientId, Context) ->
  Client = get_client(ClientId, Context),

  case Client of
    undefined ->
      {ok, ClientPid} = ws_client:start_link(),
      #{clients := Clients} = Context,
      UpdatedClients  = maps:put(ClientId, ClientPid, Clients),
      UpdatedContext  = Context#{clients => UpdatedClients},
      {ClientPid, UpdatedContext};
    _ ->
      {Client, Context}
  end.

expectation_matches_message(Message, Expectation) ->
  % get keys in Message and Expe
  % if keys in Expec contained in Message OK
  % if values for keys in Expc === values for those keys in Message OK
  % do this recursively
  MessageKeys = maps:keys(Message),
  ExpectationKeys = maps:keys(Expectation),
  AllKeysMatched = lists:all(fun (ExpectationKey) -> lists:member(ExpectationKey, MessageKeys) end, ExpectationKeys),
  case AllKeysMatched of
    true ->
      AllValuesMatches = lists:all(fun (ExpectationKey) ->
              ExpectationValue = maps:get(ExpectationKey, Expectation),
              MessageValue = maps:get(ExpectationKey, Message),
              case is_map(ExpectationValue) of
                true ->
                  expectation_matches_message(MessageValue, ExpectationValue);
                false ->
                  case ExpectationValue of
                    '_' -> true; %match all
                    _   -> ExpectationValue =:= MessageValue
                  end
              end
          end, ExpectationKeys),
      AllValuesMatches;
    false ->
      false
  end.
