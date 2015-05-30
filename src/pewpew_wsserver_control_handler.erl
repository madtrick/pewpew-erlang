-module(pewpew_wsserver_control_handler).
-export([init/1, handle/2]).

-record(pewpew_state, {pewpew_channel}).

init(Options) ->
  Worker        = proplists:get_value(worker, Options),
  {ok, Channel} = pewpew_channel:create(Worker, [{is_control, true}]),
  pewpew_core:register_control_channel(Channel),
  #pewpew_state{pewpew_channel = Channel}.

handle({close, _}, State) ->
  {close, State};
handle({pong, _}, State) ->
  {noreply, State};
handle(Message = {text, _}, State) ->
  pewpew_core:process_control_message(Message, State#pewpew_state.pewpew_channel),
  {noreply,  State}.

