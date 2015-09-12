-module(pewpew_shot_component_data).

-export([new/1]).
-export([]).
-export([update/2]).

new(Data) ->
  pewpew_map_backed_data:new(Data).

update(ShotComponentData, Data) ->
  pewpew_map_backed_data:update(ShotComponentData, Data).
