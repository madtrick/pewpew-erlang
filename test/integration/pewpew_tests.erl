-module(pewpew_tests).
-include_lib("eunit/include/eunit.hrl").

run(Config) ->
  {setup,
    fun() ->
      application:set_env(pewpew, execution_mode, test),
      pewpew:start(),
      {ok, Client}        = ws_client:start_link(),
      {ok, ControlClient} = ws_client:start_link(4321),

      #{
        ws_player_client => Client,
        ws_control_client => ControlClient
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
      Test = maps:get(test, Config),

      try maps:get(before, Config) of
        Before ->
          UpdatedContext = Before(Context),
          Test(UpdatedContext)
      catch
        _ ->
          Test(Context)
      end
    end
    }.

register_player_command_test_() ->
  run(#{
    before => fun(Context) ->
      #{ws_player_client := Client} = Context,

      ws_client:send_text(Client, <<"{\"type\":\"RegisterPlayerCommand\", \"data\":{}}">>),
      {text, Ack} = ws_client:recv(Client),
      JSON        = jiffy:decode(Ack, [return_maps]),

      Context#{json => JSON}
    end,

    test => fun(Context) ->
      #{json := JSON} = Context,

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
    before => fun(Context) ->
      #{ws_player_client := Client} = Context,

      ws_client:send_text(Client, <<"{\"type\":\"RegisterPlayerCommand\", \"data\":{}}">>),
      {text, _Ack} = ws_client:recv(Client),

      ws_client:send_text(Client, <<"{\"type\":\"RegisterPlayerCommand\", \"data\":{}}">>),
      Recv = ws_client:recv(Client),

      Context#{recv => Recv }
    end,

    test => fun(Context) ->
      #{recv := Recv} = Context,

      ?_assertEqual({error, timeout}, Recv)
    end
 }).

start_game_command_test_() ->
  run(#{
    before => fun(Context) ->
      #{ws_control_client := ControlClient} = Context,

      ws_client:send_text(ControlClient, <<"{\"type\":\"StartGameCommand\", \"data\":{}}">>),
      {text, Ack} = ws_client:recv(ControlClient),
      JSON        = jiffy:decode(Ack, [return_maps]),

      Context#{json => JSON}
    end,

    test => fun(Context) ->
      #{json := JSON} = Context,

      #{<<"type">> := AckType} = JSON,

      ?_assertEqual(<<"StartGameAck">>, AckType)
    end
 }).

reject_start_game_command_when_invalid_origin_test_() ->
  run(#{
    before => fun(Context) ->
      #{ws_player_client := Client} = Context,

      ws_client:send_text(Client, <<"{\"type\":\"StartGameCommand\", \"data\":{}}">>),
      {text, Ack} = ws_client:recv(Client),
      JSON        = jiffy:decode(Ack, [return_maps]),

      Context#{json => JSON}
    end,

    test => fun(Context) ->
      #{json := JSON} = Context,

      #{<<"type">> := AckType} = JSON,

      ?_assertEqual(<<"InvalidCommandError">>, AckType)
    end
   }).

reject_start_game_command_when_already_started_test_() ->
  run(#{
    before => fun(Context) ->
      #{ws_control_client := ControlClient} = Context,

      ws_client:send_text(ControlClient, <<"{\"type\":\"StartGameCommand\", \"data\":{}}">>),
      {text, _} = ws_client:recv(ControlClient),

      ws_client:send_text(ControlClient, <<"{\"type\":\"StartGameCommand\", \"data\":{}}">>),
      {text, Ack} = ws_client:recv(ControlClient),
      JSON        = jiffy:decode(Ack, [return_maps]),

      Context#{json => JSON}
    end,

    test => fun(Context) ->
      #{json := JSON} = Context,
      #{<<"type">> := AckType} = JSON,

      ?_assertEqual(<<"InvalidCommandError">>, AckType)
    end
   }).

send_start_game_order_to_players_test_() ->
  run(#{
    before => fun(Context) ->
      #{ws_control_client := ControlClient, ws_player_client := Client} = Context,

      ws_client:send_text(Client, <<"{\"type\":\"RegisterPlayerCommand\", \"data\":{}}">>),
      _ = ws_client:recv(Client),
      ws_client:send_text(ControlClient, <<"{\"type\":\"StartGameCommand\", \"data\":{}}">>),
      {text, StartGameOrder} = ws_client:recv(Client),
      JSON = jiffy:decode(StartGameOrder, [return_maps]),

      Context#{json => JSON}
    end,

    test => fun(Context) ->
      #{json := JSON} = Context,
      #{<<"type">> := OrderType} = JSON,

      ?_assertEqual(<<"StartGameOrder">>, OrderType)
    end
   }).