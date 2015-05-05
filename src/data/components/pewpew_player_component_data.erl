-module(pewpew_player_component_data).

-export([new/1]).
-export([id/1, x/1, y/1, origin/1, pewpew_game_context_data/1, color/1, name/1, life/1, rotation/1]).
-export([update/2]).

-record(pewpew_player_component_data, {
    id,
    x,
    y,
    origin,
    color,
    name,
    life,
    rotation,
    pewpew_game_context_data
  }).

new(Data) ->
  #pewpew_player_component_data{
    id     = proplists:get_value(id, Data),
    x      = proplists:get_value(x, Data),
    y      = proplists:get_value(y, Data),
    origin = proplists:get_value(origin, Data),
    color  = proplists:get_value(color, Data),
    name   = proplists:get_value(name, Data),
    life   = 100,
    rotation = 0,
    pewpew_game_context_data = proplists:get_value(pewpew_game_context_data, Data)
  }.

id(#pewpew_player_component_data{ id = Id }) -> Id.
x(#pewpew_player_component_data{ x = X }) -> X.
y(#pewpew_player_component_data{ y = Y }) -> Y.
color(#pewpew_player_component_data{ color = Color }) -> Color.
name(#pewpew_player_component_data{ name = Name }) -> Name.
life(#pewpew_player_component_data{ life = Life }) -> Life.
origin(#pewpew_player_component_data{ origin = Origin }) -> Origin.
rotation(#pewpew_player_component_data{ rotation = Rotation }) -> Rotation.
pewpew_game_context_data(#pewpew_player_component_data{ pewpew_game_context_data = Data }) -> Data.

update(PewpewPlayerComponentData, Data) ->
  PewpewPlayerComponentData#pewpew_player_component_data{
    x = proplists:get_value(x, Data, x(PewpewPlayerComponentData)),
    y = proplists:get_value(y, Data, y(PewpewPlayerComponentData)),
    life = proplists:get_value(life, Data, life(PewpewPlayerComponentData)),
    rotation = proplists:get_value(rotation, Data, rotation(PewpewPlayerComponentData))
  }.
