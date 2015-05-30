-module(pewpew_timer).
-behaviour(gen_server).

-export([
  start_link/0,
  stop/0,
  tick_every/2
]).

-export([
  init/1,
  handle_cast/2,
  handle_info/2,
  terminate/2
]).

-define(DEFAULT_TICK_PERIOD, 20).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?MODULE, stop).

tick_every(Module, Function) ->
  gen_server:cast(?MODULE, {add_callback, Module, Function}).

init(_) ->
  {ok, undefined, ?DEFAULT_TICK_PERIOD}.

handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast({add_callback, Module, Function}, _) ->
  {noreply, {Module, Function}, ?DEFAULT_TICK_PERIOD}.

handle_info(timeout, undefined) ->
  {noreply, undefined, ?DEFAULT_TICK_PERIOD};
handle_info(timeout, State = {Module, Function}) ->
  Module:Function(),
  {noreply, State, ?DEFAULT_TICK_PERIOD}.

terminate(_, _) ->
  ok.
