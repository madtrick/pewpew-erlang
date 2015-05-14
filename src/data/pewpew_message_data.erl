-module(pewpew_message_data).

-export([new/3, new/2]).
-export([message_module/1, message_channel/1, message_data/1]).

-record(pewpew_message_data, {
    message_module,
    message_channel,
    message_data
  }).

% Transitory function meanwhile we don't create all the
% messages passing the channel
new(MessageModule, MessageData) ->
  new(MessageModule, undefined, MessageData).

new(MessageModule, Channel, MessageData) ->
  #pewpew_message_data{
    message_module  = MessageModule,
    message_channel = Channel,
    message_data    = MessageData
  }.

message_module(#pewpew_message_data{ message_module = MessageModule }) -> MessageModule.
message_channel(#pewpew_message_data{ message_channel = MessageChannel }) -> MessageChannel.
message_data(#pewpew_message_data{ message_data = MessageData }) -> MessageData.
