-module(pewpew_message_dispatcher).

-export([dispatch/1]).

dispatch([]) ->
  ok;
dispatch([Delivery | Deliveries]) ->
  {DispatchRule, Channels, Messages} = Delivery,
  JSONBody       = convert_to_json(Messages),

  lager:debug("Dispatch rule ~w", [DispatchRule]),
  dispatch_with_rule(DispatchRule, JSONBody, Channels),
  dispatch( Deliveries).


dispatch_with_rule(Rule, Data, Channel) when not is_list(Channel)->
  dispatch_with_rule(Rule, Data, [Channel]);
dispatch_with_rule(send_to, Data, Channels) ->
  pewpew_multicast:publish(Data, Channels);
dispatch_with_rule(_, _, _) ->
  lager:debug("Unknown dispatch rule").

convert_to_json(Messages) ->
  Structs = lists:map(fun(Message) ->
    MessageModule = pewpew_message_data:message_module(Message),
    MessageData   = pewpew_message_data:message_data(Message),

    MessageModule:toJSON(MessageData)
  end, Messages),

  jiffy:encode(Structs).
