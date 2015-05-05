-module(pewpew_event_bus_data).

-export([new/1]).
-export([events/1]).
-export([update/2]).

-record(pewpew_event_bus_data,{
    events
  }).

new(_) ->
  #pewpew_event_bus_data{
    events = []
  }.

events(#pewpew_event_bus_data{ events = Events }) -> Events.

update(PewpewEventBusData, Options) ->
  PewpewEventBusData#pewpew_event_bus_data{
    events = proplists:get_value(events, Options, events(PewpewEventBusData))
  }.

