-module(pewpew_game_update_notification_context).
-include_lib("eunit/include/eunit.hrl").

-export([call/1]).

call(NotificationContextData) ->
  PewPewGame = pewpew_notification_context_data:pewpew_game(NotificationContextData),
  IsGameStarted = pewpew_game:is_started(PewPewGame),

  case IsGameStarted of
    false ->
      [];
    true ->
      Updates = pewpew_game:update(PewPewGame),
      lists:map(fun(Update) ->
        case Update of
          {player, _, no_update} ->
            noreply; %ignore this
          {player, PlayerChannel, update, U} ->
            case U of
              {notification, Notification} ->
                %PlayerChannel = pewpew_player_component:channel(Player),
                {reply, [{send_to, PlayerChannel, Notification}]}
            end;
          {player, PlayerChannel, destroyed, U} ->
              ?debugMsg("Player destroyed notification"),
              {notification, Notification} = U,
              %PlayerChannel = pewpew_player_component:channel(Player),
              {reply, [{send_to, PlayerChannel, Notification}]};
          {shot, _, update, _} ->
            noreply; %ignore them for now
          do_nothing ->
            noreply %ignore this
        end
      end, Updates)
  end.
