-module(pewpew_notification_context_data).

-export([
  new/1,
  pewpew_game/1
]).

new(Options) ->
  pewpew_map_backed_data:new(Options).

pewpew_game(#{ pewpew_game := Value }) -> Value.
