-module(pewpew_test_support).
-include_lib("eunit/include/eunit.hrl").

-export([
  run_test/1,
  ws_client_send/2,
  ws_client_recv/1,
  generate_reject_move_command_test/1,
  generate_valid_move_command_test/1
]).

run_test(Config) ->
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

ws_client_recv(ClientId) ->
  fun(Context) ->
    Client = maps:get(ClientId, Context),
    {text, Reply} = ws_client:recv(Client),
    {reply, Reply}
  end.

ws_client_send(ClientId, Message) ->
  JSON = maybe_convert_message_to_json(Message),

  fun(Context) ->
      Client = maps:get(ClientId, Context),

      ws_client:send_text(Client, JSON),
      {text, Reply} = ws_client:recv(Client),
      {reply, Reply}
  end.

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

generate_move_command(Options) ->
  #{
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
      ws_client_send(ws_player_client, <<"{\"type\":\"RegisterPlayerCommand\", \"data\":{}}">>),
      SetPlayerCoordinates,
      ws_client_send(ws_control_client, <<"{\"type\":\"StartGameCommand\", \"data\":{}}">>),
      GetPlayerCoordinates(coordinates_before_move),
      ws_client_recv(ws_player_client),
      ws_client_send(ws_player_client, <<"{\"type\":\"MovePlayerCommand\", \"data\":", JSONifiedMovements/binary, "}">>),
      GetPlayerCoordinates(coordinates_after_move)
    ]
    end,

    test => Test
   }).

maybe_convert_message_to_json(Message) when is_map(Message) ->
  jiffy:encode(Message);
maybe_convert_message_to_json(Message) ->
  Message.
