-module(pewpew_player_component_data).

-export([new/1]).
-export([
  id/1,
  x/1,
  y/1,
  origin/1,
  color/1,
  name/1,
  hits/1,
  life/1,
  rotation/1,
  radius/1,
  radar_config_data/1,
  shooting_info/1,
  speed/1
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
hits(#{ hits := Hits }) -> Hits.
life(#{ life := Life }) -> Life.
origin(#{ origin := Origin }) -> Origin.
rotation(#{ rotation := Rotation }) -> Rotation.
radius(#{ radius := Value }) -> Value.
radar_config_data(#{ radar_config_data := Value }) -> Value.
shooting_info(#{ shooting_info := Value }) -> Value.
speed(#{ speed := Value }) -> Value.

update(PewPewPlayerComponentData, Data) ->
  pewpew_map_backed_data:update(PewPewPlayerComponentData, Data).
