-module(pewpew_game_context_data).

-export([new/1]).
-export([pewpew_event_bus/1]).

-record(pewpew_game_context_data, {
    pewpew_event_bus
  }).

new(Options) ->
  #pewpew_game_context_data{
    pewpew_event_bus = proplists:get_value(pewpew_event_bus, Options)
  }.

pewpew_event_bus(#pewpew_game_context_data{ pewpew_event_bus = PewpewEventBus }) -> PewpewEventBus.
