-module(pewpew_channel).
-behaviour(gen_server).

-export([close/1, create/1, send/2]).
-export([init/1, handle_cast/2, handle_info/2, terminate/2]).

-record(pewpew_channel_state, {
    wsserver_worker
  }).

-define(HEROKU_KEEP_ALIVE_TIMEOUT, 20*1000). %20 seconds

close(Channel) ->
  gen_server:cast(Channel, close).

create(WSWorker) ->
  gen_server:start_link(?MODULE, WSWorker, []).

send(Channel, Data) ->
  gen_server:cast(Channel, {send, Data}).

init(WSWorker) ->
  pewpew_registry:register(self()),
  {ok, #pewpew_channel_state{ wsserver_worker = WSWorker }, ?HEROKU_KEEP_ALIVE_TIMEOUT}.

handle_info(timeout, State) ->
  wsserver_worker_websocket:ping(State#pewpew_channel_state.wsserver_worker),
  {noreply, State, ?HEROKU_KEEP_ALIVE_TIMEOUT}.

handle_cast(close, State) ->
  wsserver_worker_websocket:close(State#pewpew_channel_state.wsserver_worker),
  {stop, channel_close, State};
handle_cast({send, Data}, State) ->
  wsserver_worker_websocket:send(State#pewpew_channel_state.wsserver_worker, Data),
  {noreply, State, ?HEROKU_KEEP_ALIVE_TIMEOUT}.

terminate(_Reason, _State) ->
  die.
