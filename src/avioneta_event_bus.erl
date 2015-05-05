-module(avioneta_event_bus).
-behaviour(gen_event).

-export([start_link/0, on/3, trigger/2, trigger/3]).
-export([init/1, handle_call/2, handle_event/2]).

start_link() ->
  {ok, EventMgr} = gen_event:start_link(),
  gen_event:add_handler(EventMgr, ?MODULE, []),
  {ok, EventMgr}.

on(EventMgr, EventName, EventCallback) ->
  gen_event:call(EventMgr, ?MODULE, {on, EventName, EventCallback}).

trigger(EventMgr, EventName) ->
  trigger(EventMgr, {EventName, avioneta_event_bus_no_params}).
trigger(EventMgr, EventName, Data) ->
  gen_event:notify(EventMgr, {event, EventName, params, Data}).

init(_) ->
  {ok, avioneta_event_bus_data:new([])}.

handle_call({on, EventName, EventCallback}, EventBusData) ->
  Events = avioneta_event_bus_data:events(EventBusData),
  {ok, cona, avioneta_event_bus_data:update(EventBusData, [{events, [{EventName, EventCallback} | Events]}])}.

handle_event({event, Event, params, avioneta_event_bus_no_params}, EventBusData) ->
  lager:debug("Handling event"),
  lager:debug("~w", [EventBusData]),
  execute_callbacks(event_callbacks(EventBusData, Event), []),
  {ok, EventBusData};
handle_event({event, Event, params, Data}, EventBusData) ->
  lager:debug("Handling event"),
  lager:debug("~w", [EventBusData]),
  execute_callbacks(event_callbacks(EventBusData, Event), Data),
  {ok, EventBusData}.

execute_callbacks(Callbacks, Params) ->
  [apply(Callback, Params) || Callback <- Callbacks].

event_callbacks(EventBusData, Event) ->
  [Callback || {_, Callback} <- avioneta_event_bus_data:events(EventBusData)].
