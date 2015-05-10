-module(pewpew_core).
-behaviour(gen_server).

-export([start_link/0, process_message/2]).
-export([init/1, handle_cast/2, handle_call/3, terminate/2]).

% Testing only
-export([number_of_pending_messages/0, number_of_pending_messages_per_channel/1]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

process_message(Message, OriginChannel) ->
  gen_server:cast(?MODULE, {process_message, OriginChannel, Message}).

number_of_pending_messages() ->
  gen_server:call(?MODULE, number_of_pending_messages).

number_of_pending_messages_per_channel(Channel) ->
  gen_server:call(?MODULE, {number_of_pending_messages_per_channel, Channel}).

init(_) ->
  {ok, build_pewpew_core_state()}.

handle_cast({disconnect_player, _OriginChannel}, _State) ->
  ok;
handle_cast({process_message, OriginChannel, Message}, State) ->
  NewPewpewCoreStateData = handle_process_message(OriginChannel, Message, State),
  {noreply, NewPewpewCoreStateData}.

handle_call(number_of_pending_messages, _, State) ->
  PendingMessages = pewpew_core_state_data:pending_messages(State),
  NumberOfPendingMessages = internal_number_of_pending_messages(PendingMessages),
  %PendingMessages = pewpew_core_state_data:pending_messages(State),
  {reply, NumberOfPendingMessages, State};
handle_call({number_of_pending_messages_per_channel, Channel}, _, State) ->
  PendingMessages = pewpew_core_state_data:pending_messages(State),
  NumberOfPendingMessages = internal_number_of_pending_messages_per_channel(Channel, PendingMessages),
  {reply, NumberOfPendingMessages, State}.

handle_process_message(OriginChannel, Message ={text, _}, State) ->
  %CommandContexts = pewpew_command_parser:parse(Message),
  %evaluate_command_return_values(
  %  pewpew_command_runner:run(CommandContexts, pewpew_game(State), OriginChannel),
  %  OriginChannel
  %),
  PendingMessages            = pewpew_core_state_data:pending_messages(State),
  UpdatedPendingMessagesList = maybe_update_pending_messages_list(OriginChannel, PendingMessages, Message),
  UpdatedState               = pewpew_core_state_data:update(State, [{pending_messages, UpdatedPendingMessagesList}]),
  UpdatedState.

internal_number_of_pending_messages(PendingMessages) ->
  erlang:length(PendingMessages).

internal_number_of_pending_messages_per_channel(Channel, PendingMessages) ->
  PendingMessagesForChannel = pending_messages_per_channel(Channel, PendingMessages),
  internal_number_of_pending_messages(PendingMessagesForChannel).

pending_messages_per_channel(Channel, PendingMessages) ->
  proplists:get_value(Channel, PendingMessages, []).

maybe_update_pending_messages_list(Channel, PendingMessages, Message, []) ->
  [{Channel, [Message]} | PendingMessages];
maybe_update_pending_messages_list(_, PendingMessages, _, _) ->
  PendingMessages. %Discard the message

maybe_update_pending_messages_list(Channel, PendingMessages, Message) ->
  PendingMessagesForChannel = pending_messages_per_channel(Channel, PendingMessages),
  maybe_update_pending_messages_list(Channel, PendingMessages, Message, PendingMessagesForChannel).


evaluate_command_return_values([], _) ->
  ok;
evaluate_command_return_values([ReturnValue |  Tail], OriginChannel) ->
  case ReturnValue of
    noreply ->
      evaluate_command_return_values(Tail, OriginChannel);
    {reply, Messages} ->
      dispatch_messages(Messages, OriginChannel, filter_origin_channel(OriginChannel, all_channels())),
      evaluate_command_return_values(Tail, OriginChannel);
    close ->
      pewpew_channel:close(OriginChannel),
      ok; %Discard all pending values
    {close, Messages} ->
      dispatch_messages(Messages, OriginChannel, filter_origin_channel(OriginChannel, all_channels())),
      pewpew_channel:close(OriginChannel),
      ok %Discard all pending values
  end.

dispatch_messages(Messages, OriginChannel, OtherChannels) ->
  pewpew_message_dispatcher:dispatch(Messages, OriginChannel, OtherChannels ).

filter_origin_channel(OriginChannel, Channels) ->
  lists:filter(fun(Element) -> Element =/= OriginChannel end, Channels).

all_channels() ->
  pewpew_registry:entries().

terminate(_Reason, _State) ->
  die.

build_pewpew_core_state() ->
  GameName = random_game_name(),
  pewpew_games_sup:add_game(GameName),
  pewpew_core_state_data:new(GameName).

pewpew_game(State) ->
  pewpew_core_state_data:pewpew_game(State).

random_game_name() ->
  %
  % PLEASE NOTE that if the server stays up for too long we might run out of memory
  % if the atom table grows. Right now we have to stick with atoms as is the only valid
  % value for locally registering processes. In the future we might migrate to a global
  % registry
  %
  erlang:list_to_atom("game-" ++ erlang:integer_to_list(fserlangutils_time:microseconds_since_epoch())).
