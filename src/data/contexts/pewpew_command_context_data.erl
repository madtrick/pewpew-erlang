-module(avioneta_command_context_data).

-export([new/2, context/1, command_data/1, avioneta_game/1, origin/1]).
-export([update/2, origin/2]).

-record(avioneta_command_context_data, {
    context,
    command_data,
    avioneta_game,
    origin
  }).


new(Context, CommandData) ->
  #avioneta_command_context_data{
    context = Context,
    command_data = CommandData
  }.

context(#avioneta_command_context_data{ context = Context }) -> Context.
command_data(#avioneta_command_context_data{ command_data = CommandData }) -> CommandData.
avioneta_game(#avioneta_command_context_data{ avioneta_game = AvionetaGame }) -> AvionetaGame.
origin(#avioneta_command_context_data{ origin = Origin }) -> Origin.


update(AvionetaCommandContextData, Data) ->
  AvionetaCommandContextData#avioneta_command_context_data{
    avioneta_game = proplists:get_value( avioneta_game, Data, AvionetaCommandContextData#avioneta_command_context_data.avioneta_game ),
    origin = proplists:get_value(origin, Data, AvionetaCommandContextData#avioneta_command_context_data.origin)
  }.

origin(AvionetaCommandContextData, Origin) ->
  AvionetaCommandContextData#avioneta_command_context_data{ origin = Origin }.
