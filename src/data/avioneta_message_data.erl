-module(avioneta_message_data).

-export([new/2]).
-export([message_module/1, message_data/1]).

-record(avioneta_message_data, {
    message_module,
    message_data
  }).

new(MessageModule, MessageData) ->
  #avioneta_message_data{
    message_module = MessageModule,
    message_data = MessageData
  }.

message_module(#avioneta_message_data{ message_module = MessageModule }) -> MessageModule.
message_data(#avioneta_message_data{ message_data = MessageData }) -> MessageData.
