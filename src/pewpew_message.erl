-module(pewpew_message).

-export([new/1, new/2]).
-export([data/1, serializer/1]).

new(Serializer) ->
  new(Serializer, undefined).
new(Serializer, Data) ->
  Values = [
      {serializer, Serializer},
      {data, Data}
      ],

  pewpew_dataset:new(Values).

data(Message) ->
  pewpew_dataset:get(data, Message).

serializer(Message) ->
  pewpew_dataset:get(serializer, Message).
