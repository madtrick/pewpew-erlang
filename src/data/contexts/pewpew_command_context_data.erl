-module(pewpew_command_context_data).

-export([new/2, context/1, command_data/1, pewpew_game/1, origin/1]).
-export([update/2, origin/2]).

-record(pewpew_command_context_data, {
    context,
    command_data,
    pewpew_game,
    origin
  }).


new(Context, CommandData) ->
  #pewpew_command_context_data{
    context = Context,
    command_data = CommandData
  }.

context(#pewpew_command_context_data{ context = Context }) -> Context.
command_data(#pewpew_command_context_data{ command_data = CommandData }) -> CommandData.
pewpew_game(#pewpew_command_context_data{ pewpew_game = PewpewGame }) -> PewpewGame.
origin(#pewpew_command_context_data{ origin = Origin }) -> Origin.


update(PewpewCommandContextData, Data) ->
  PewpewCommandContextData#pewpew_command_context_data{
    pewpew_game = proplists:get_value( pewpew_game, Data, PewpewCommandContextData#pewpew_command_context_data.pewpew_game ),
    origin = proplists:get_value(origin, Data, PewpewCommandContextData#pewpew_command_context_data.origin)
  }.

origin(PewpewCommandContextData, Origin) ->
  PewpewCommandContextData#pewpew_command_context_data{ origin = Origin }.
