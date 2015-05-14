-module(pewpew_tests).
-include_lib("eunit/include/eunit.hrl").

register_player_command_test_() ->
  {setup,
    fun() ->
        application:set_env(pewpew, execution_mode, test),
        pewpew:start(),
        {ok, Client} = ws_client:start_link(),
        Client
    end,
    fun(Client) ->
        ws_client:stop(Client),
        pewpew:stop()
    end,
    fun(Client) ->
        ws_client:send_text(Client, <<"{\"type\":\"RegisterPlayerCommand\", \"data\":{}}">>),
        {text, Ack} = ws_client:recv(Client),
        JSON = jiffy:decode(Ack, [return_maps]),

        #{<<"type">> := AckType} = JSON,

        ?_assertEqual(<<"RegisterPlayerAck">>, AckType)
    end}.
