-module(avioneta_event_bus_data).

-export([new/1]).
-export([events/1]).
-export([update/2]).

-record(avioneta_event_bus_data,{
    events
  }).

new(_) ->
  #avioneta_event_bus_data{
    events = []
  }.

events(#avioneta_event_bus_data{ events = Events }) -> Events.

update(AvionetaEventBusData, Options) ->
  AvionetaEventBusData#avioneta_event_bus_data{
    events = proplists:get_value(events, Options, events(AvionetaEventBusData))
  }.

