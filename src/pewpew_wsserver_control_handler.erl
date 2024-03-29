-module(pewpew_wsserver_control_handler).
-export([init/1, handle/2]).

-record(pewpew_state, {pewpew_channel}).

init(Options) ->
  lager:info("websockets control handler created"),
  Worker        = proplists:get_value(worker, Options),
  {ok, Channel} = pewpew_channel:create(Worker, [{is_control, true}]),
  pewpew_core:register_control_channel(Channel),
  #pewpew_state{pewpew_channel = Channel}.

handle(connection_close, State) ->
  lager:info("websockets control handler stopped"),
  pewpew_channel:exit(State#pewpew_state.pewpew_channel),
  ok;
handle({close, _}, State) ->
  lager:info("websockets control handler stopped"),
  pewpew_channel:exit(State#pewpew_state.pewpew_channel),
  {close, State};
handle({pong, _}, State) ->
  {noreply, State};
handle(Message = {text, _}, State) ->
  pewpew_core:process_control_message(Message, State#pewpew_state.pewpew_channel),
  {noreply,  State}.

