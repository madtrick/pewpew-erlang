-module(pewpew_core_state_data).

-export([
  new/1,
  update/2,
  pewpew_game/1,
  pending_messages/1,
  control_channel/1
]).

new(PewPewGame) ->
  Options = [
     {pewpew_game, PewPewGame},
     {pending_messages, []},
     {control_channel, undefined}
  ],
  pewpew_map_backed_data:new(Options).

pending_messages(#{ pending_messages := Value }) -> Value.
pewpew_game(#{ pewpew_game := Value }) -> Value.
control_channel(#{ control_channel := Value }) -> Value.

update(Data, Options) ->
  pewpew_map_backed_data:update(Data, Options).
