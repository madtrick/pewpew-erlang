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

        #{<<"type">> := AckType, <<"data">> := Data} = JSON,
        #{<<"id">> := Id, <<"x">> := X, <<"y">> := Y, <<"life">> := Life} = Data,

        ?_assertEqual(<<"RegisterPlayerAck">>, AckType),
        ?_assert(is_integer(Id)),
        ?_assert(is_integer(X)),
        ?_assert(is_integer(Y)),
        ?_assert(is_integer(Life))
    end}.
