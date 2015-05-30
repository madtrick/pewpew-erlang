-module(pewpew_core_state_data).

-export([new/1, pewpew_game/1, pending_messages/1]).
-export([update/2]).

new(PewPewGame) ->
  Options = [
     {pewpew_game, PewPewGame},
     {pending_messages, []}
  ],
  pewpew_map_backed_data:new(Options).

pending_messages(#{ pending_messages := Value }) -> Value.
pewpew_game(#{ pewpew_game := Value }) -> Value.

update(Data, Options) ->
  pewpew_map_backed_data:update(Data, Options).
