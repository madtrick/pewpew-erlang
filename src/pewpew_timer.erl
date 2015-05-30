-module(pewpew_timer).
-behaviour(gen_server).

-export([
  start_link/0,
  stop/0,
  tick_every/1,
  tick_every/2,
  remove_callback/1
]).

-export([
  init/1,
  handle_cast/2,
  handle_info/2,
  terminate/2
]).

-define(DEFAULT_TICK_PERIOD, 20).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?MODULE, stop).

tick_every(Fun) ->
  gen_server:cast(?MODULE, {add_callback, Fun}).
tick_every(Module, Function) ->
  gen_server:cast(?MODULE, {add_callback, {Module, Function}}).

remove_callback(Fun) ->
  gen_server:cast(?MODULE, {remove_callback, Fun}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_) ->
  {ok, [], ?DEFAULT_TICK_PERIOD}.

handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast({remove_callback, Callback}, Callbacks) ->
  NewCallbacksList = remove_callback(Callback, Callbacks),
  {noreply, NewCallbacksList};
handle_cast({add_callback, Callback}, Callbacks) ->
  {noreply, [Callback | Callbacks], ?DEFAULT_TICK_PERIOD}.

handle_info(timeout, Callbacks) ->
  ok = call_callbacks(Callbacks),
  {noreply, Callbacks, ?DEFAULT_TICK_PERIOD}.

terminate(_, _) ->
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
call_callbacks([]) ->
  ok;
call_callbacks([{Module, Function} | Tail]) ->
  spawn(Module, Function, []),
  call_callbacks(Tail);
call_callbacks([Fun | Tail]) ->
  spawn(Fun),
  call_callbacks(Tail).

remove_callback(_, []) ->
  [];
remove_callback(Needle, [Callback | Tail]) when Needle =:= Callback ->
  Tail.
