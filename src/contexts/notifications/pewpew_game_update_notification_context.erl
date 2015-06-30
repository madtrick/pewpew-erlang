-module(pewpew_game_update_notification_context).
-include_lib("eunit/include/eunit.hrl").

-export([call/1]).

call(NotificationContextData) ->
  % TODO do not update the game before the game starts
  PewPewGame = pewpew_notification_context_data:pewpew_game(NotificationContextData),
  Updates = pewpew_game:update(PewPewGame),
  ?debugVal(Updates),


  lists:map(fun(Update) ->
                ?debugVal(Update),
    case Update of
      {player, Player, update, U} ->
        case U of
          {notification, Notification} ->
            PlayerChannel = pewpew_player_component:channel(Player),
            {reply, [{send_to, PlayerChannel, [Notification]}]}
        end
    end
  end, Updates).
