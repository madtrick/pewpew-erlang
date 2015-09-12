-module(pewpew_shot_component_data).

-export([new/1]).
-export([x/1, y/1]).
-export([update/2]).

new(Data) ->
  pewpew_map_backed_data:new(Data).

x(#{ x := Value }) -> Value.
y(#{ y := Value }) -> Value.

update(ShotComponentData, Data) ->
  pewpew_map_backed_data:update(ShotComponentData, Data).
