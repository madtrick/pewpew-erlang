-module(avioneta_game_context_data).

-export([new/1]).
-export([avioneta_event_bus/1]).

-record(avioneta_game_context_data, {
    avioneta_event_bus
  }).

new(Options) ->
  #avioneta_game_context_data{
    avioneta_event_bus = proplists:get_value(avioneta_event_bus, Options)
  }.

avioneta_event_bus(#avioneta_game_context_data{ avioneta_event_bus = AvionetaEventBus }) -> AvionetaEventBus.
