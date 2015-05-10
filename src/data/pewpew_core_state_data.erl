-module(pewpew_core_state_data).

-export([new/1, pewpew_game/1, pending_messages/1]).
-export([update/2]).

-record(pewpew_core_state_data, {
    pewpew_game,
    pending_messages = []
  }).

new(PewpewGame) ->
  #pewpew_core_state_data{
    pewpew_game = PewpewGame
  }.

pending_messages(#pewpew_core_state_data{ pending_messages = PendingMessages }) ->
  PendingMessages.
pewpew_game(#pewpew_core_state_data{ pewpew_game = PewpewGame }) ->
  PewpewGame.

update(PewpewCoreStateData, Options) ->
  PewpewCoreStateData#pewpew_core_state_data{
    pending_messages = proplists:get_value(pending_messages, Options, pending_messages(PewpewCoreStateData))
  }.
