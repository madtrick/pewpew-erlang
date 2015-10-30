-module(pewpew_channel).
-behaviour(gen_server).

-export([
  config/1,
  close/1,
  create/1,
  create/2,
  send/2,
  exit/1
]).
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2
]).

-record(pewpew_channel_state, {
    wsserver_worker,
    config
  }).

-define(HEROKU_KEEP_ALIVE_TIMEOUT, 20*1000). %20 seconds

config(Channel) ->
  gen_server:call(Channel, config).

close(Channel) ->
  gen_server:cast(Channel, close).

create(WSWorker) ->
  create(WSWorker, []).
create(WSWorker, Config) ->
  gen_server:start_link(?MODULE, {WSWorker, Config}, []).

send(Channel, Data) ->
  gen_server:cast(Channel, {send, Data}).

exit(Channel) ->
  gen_server:cast(Channel, exit).

init(Options) ->
  {WSWorker, Config} = Options,
  State = #pewpew_channel_state{
     wsserver_worker = WSWorker,
     config = Config
  },
  pewpew_registry:register(self()),

  {ok, State, ?HEROKU_KEEP_ALIVE_TIMEOUT}.

handle_call(config, _, State) ->
  Config = State#pewpew_channel_state.config,
  {reply, Config, State}.

handle_info(timeout, State) ->
  wsserver_worker_websocket:ping(State#pewpew_channel_state.wsserver_worker),
  {noreply, State, ?HEROKU_KEEP_ALIVE_TIMEOUT}.

handle_cast(close, State) ->
  wsserver_worker_websocket:close(State#pewpew_channel_state.wsserver_worker),
  {stop, normal, State};
handle_cast({send, Data}, State) ->
  wsserver_worker_websocket:send(State#pewpew_channel_state.wsserver_worker, Data),
  {noreply, State, ?HEROKU_KEEP_ALIVE_TIMEOUT};
handle_cast(exit, State) ->
  {stop, normal, State}.

terminate(_Reason, _State) ->
  die.
