-module(pewpew_player_component_data).

-export([new/1]).
-export([
  id/1,
  x/1,
  y/1,
  origin/1,
  pewpew_game_context_data/1,
  color/1,
  name/1,
  life/1,
  rotation/1,
  radius/1,
  radar_component/1
]).
-export([update/2]).

new(Data) ->
  Options = lists:append([{life, 100}, {rotation, 0}],  Data),
  pewpew_map_backed_data:new(Options).

id(#{ id := Id }) -> Id.
x(#{ x := X }) -> X.
y(#{ y := Y }) -> Y.
color(#{ color := Color }) -> Color.
name(#{ name := Name }) -> Name.
life(#{ life := Life }) -> Life.
origin(#{ origin := Origin }) -> Origin.
rotation(#{ rotation := Rotation }) -> Rotation.
pewpew_game_context_data(#{ pewpew_game_context_data := Data }) -> Data.
radius(#{ radius := Value }) -> Value.
radar_component(#{ radar_component := Value }) -> Value.

update(PewPewPlayerComponentData, Data) ->
  pewpew_map_backed_data:update(PewPewPlayerComponentData, Data).
