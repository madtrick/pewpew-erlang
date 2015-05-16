-module(pewpew_wsserver_player_handler).
-export([init/1, handle/2]).

-record(pewpew_state, {pewpew_channel}).

init(Options) ->
  Worker        = proplists:get_value(worker, Options),
  {ok, Channel} = pewpew_channel:create(Worker, [{is_control, false}]),
  #pewpew_state{pewpew_channel = Channel}.

handle({close, _}, State) ->
  {close, State};
handle({pong, _}, State) ->
  {noreply, State};
handle(Message = {text, _}, State) ->
  pewpew_core:process_player_message(Message, State#pewpew_state.pewpew_channel),
  {noreply,  State}.
