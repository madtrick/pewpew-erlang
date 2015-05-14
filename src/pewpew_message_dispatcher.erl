-module(pewpew_message_dispatcher).

-export([dispatch/2]).

dispatch(Deliveries, Others) ->
  dispatch_messages(Deliveries, Others).

dispatch_messages([], _) ->
  done;
dispatch_messages([Delivery | Deliveries], OtherChannels) ->
  {DispatchRule, Message} = Delivery,

  MessageChannel = pewpew_message_data:message_channel(Message),
  JSONBody       = convert_to_json(Message),

  lager:debug("Dispatch rule ~w", [DispatchRule]),
  dispatch_with_rule(DispatchRule, JSONBody, MessageChannel, OtherChannels),
  dispatch_messages( Deliveries, OtherChannels).

dispatch_with_rule(send_to_origin, Data, MessageChannel, _OtherChannels) ->
  pewpew_multicast:publish(Data, MessageChannel);
dispatch_with_rule(send_to_others, Data, _MessageChannel, OtherChannels) ->
  pewpew_multicast:publish(Data, OtherChannels);
dispatch_with_rule(send_to_all, Data, MessageChannel, OtherChannels) ->
  pewpew_multicast:publish(Data, [MessageChannel | OtherChannels]);
dispatch_with_rule(_, _, _, _) ->
  lager:debug("Unknown dispatch rule").

convert_to_json(Message) ->
  MessageModule = pewpew_message_data:message_module(Message),
  MessageData   = pewpew_message_data:message_data(Message),

  MessageModule:toJSON(MessageData).
