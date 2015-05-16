-module(pewpew_core).
-behaviour(gen_server).

-export([
  start_link/0,
  process_player_message/2,
  process_control_message/2
]).
-export([
  init/1,
  handle_cast/2,
  handle_call/3,
  terminate/2
]).

% Testing only
-export([
  number_of_pending_messages/0,
  number_of_pending_messages_per_channel/1,
  next_cycle/0
]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

process_control_message(Message, OriginChannel) ->
  gen_server:cast(?MODULE, {process_control_message, OriginChannel, Message}).

process_player_message(Message, OriginChannel) ->
  gen_server:cast(?MODULE, {process_player_message, OriginChannel, Message}).

next_cycle() ->
  gen_server:call(?MODULE, next_cycle).

number_of_pending_messages() ->
  gen_server:call(?MODULE, number_of_pending_messages).

number_of_pending_messages_per_channel(Channel) ->
  gen_server:call(?MODULE, {number_of_pending_messages_per_channel, Channel}).

init(_) ->
  pewpew_timer:tick_every(?MODULE, next_cycle),
  {ok, build_pewpew_core_state()}.

handle_cast({disconnect_player, _OriginChannel}, _State) ->
  ok;
handle_cast({process_control_message, Channel, {text, Message}}, State) ->
  Game                  = pewpew_game(State),
  CommandContext        = pewpew_command_parser:parse(Message),
  UpdatedCommandContext = pewpew_command_context_data:update(CommandContext, [{origin, Channel}, {pewpew_game, Game}]),
  Reply                 = pewpew_command_runner:run(UpdatedCommandContext),
  send_replies([Reply]),
  {noreply, State};
handle_cast({process_player_message, OriginChannel, Message}, State) ->
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
  {reply, NumberOfPendingMessages, State};
handle_call(next_cycle, _, State) ->
  PendingMessages              = pewpew_core_state_data:pending_messages(State),
  ReversedPendingMessages      = lists:reverse(PendingMessages),
  {_UpdatedGameState, Replies} = next_cycle(ReversedPendingMessages, pewpew_game(State)),
  %UpdatedState                = pewpew_core_state_data:update(State, [{pending_messages, UpdatedPendingMessagesList}]),
  %{reply, ok, UpdatedState}.
  ok = send_replies(Replies),
  {reply, ok, State}.

handle_process_message(OriginChannel, {text, Message}, State) ->
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
  % TODO: check that inside the message there's only one command and not
  % a more that one command packaged inside one message. This might mean
  % parsin the messages at this point
  [{Channel, [Message]} | PendingMessages];
maybe_update_pending_messages_list(_, PendingMessages, _, _) ->
  PendingMessages. %Discard the message

maybe_update_pending_messages_list(Channel, PendingMessages, Message) ->
  PendingMessagesForChannel = pending_messages_per_channel(Channel, PendingMessages),
  maybe_update_pending_messages_list(Channel, PendingMessages, Message, PendingMessagesForChannel).

next_cycle(Messages, GameState) ->
  % TODO:
  % - update state after evaluating a message
  evaluate_messages(Messages, GameState).

evaluate_messages(Messages, GameState) ->
  evaluate_messages(Messages, GameState, []).

evaluate_messages([], GameState, Replies) ->
  {GameState, lists:reverse(Replies)};
evaluate_messages([MessagesPerChannel | Tail], GameState, Replies) ->
  % TODO:
  % - pass the valid origin channel

  {Channel, [Message]}  = MessagesPerChannel,
  CommandContext        = pewpew_command_parser:parse(Message),
  UpdatedCommandContext = pewpew_command_context_data:update(CommandContext, [{origin, Channel}, {pewpew_game, GameState}]),
  %Reply                = evaluate_command_return_values(
  %  pewpew_command_runner:run(CommandContexts, GameState, origin),
  %  origin
  %),
  Reply = pewpew_command_runner:run(UpdatedCommandContext),

  evaluate_messages(Tail, GameState, [Reply | Replies]).

send_replies([]) ->
  ok;
send_replies([ReturnValue |  Tail]) ->
  case ReturnValue of
    noreply ->
      send_replies(Tail);
    {reply, Messages} ->
      dispatch_messages(Messages, all_channels_placeholder),
      send_replies(Tail);
    close ->
      pewpew_channel:close(channel_placeholder),
      ok; %Discard all pending values
    {close, Messages} ->
      dispatch_messages(Messages, all_channels_placeholder),
      pewpew_channel:close(channel_placeholder),
      ok %Discard all pending values
  end.

dispatch_messages(Messages, OtherChannels) ->
  pewpew_message_dispatcher:dispatch(Messages, OtherChannels ).

%filter_origin_channel(OriginChannel, Channels) ->
%  lists:filter(fun(Element) -> Element =/= OriginChannel end, Channels).

%all_channels() ->
%  pewpew_registry:entries().

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
