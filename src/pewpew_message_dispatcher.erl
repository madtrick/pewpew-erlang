-module(pewpew_message_dispatcher).

-export([dispatch/3]).

dispatch(Messages, Origin, Others) ->
  dispatch_messages(Messages, Origin, Others).

dispatch_messages([], _, _) ->
  done;
dispatch_messages([Message | Messages], OriginChannel, OtherChannels) ->
  {DispatchRule, Messages2} = Message,

  lager:debug("Dispatch rule ~w", [DispatchRule]),
  dispatch_with_rule(DispatchRule, convert_to_json(Messages2), OriginChannel, OtherChannels),
  dispatch_messages( Messages, OriginChannel, OtherChannels).

dispatch_with_rule(send_to_origin, Data, OriginChannel, _OtherChannels) ->
  pewpew_multicast:publish(Data, OriginChannel);
dispatch_with_rule(send_to_others, Data, _OriginChannel, OtherChannels) ->
  pewpew_multicast:publish(Data, OtherChannels);
dispatch_with_rule(send_to_all, Data, OriginChannel, OtherChannels) ->
  pewpew_multicast:publish(Data, [OriginChannel | OtherChannels]);
dispatch_with_rule(_, _, _, _) ->
  lager:debug("Unknown dispatch rule").

convert_to_json(Messages) ->
  JSONMessages = lists:foldl(fun(MessageData, Acc) ->
      [ (pewpew_message_data:message_module(MessageData)):toJSON(
        pewpew_message_data:message_data(MessageData)
      ) | Acc]
  end, [], Messages),

  ["[" ++ join(JSONMessages, ",") ++ "]"].

join([], _) -> [];
join([List|Lists], Separator) ->
     lists:flatten([List | [[Separator,Next] || Next <- Lists]]).
