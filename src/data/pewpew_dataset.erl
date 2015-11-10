-module(pewpew_dataset).

-export([
  new/1,
  get/2,
  update/2
]).

new(Values) ->
  pewpew_map_backed_data:new(Values).

get(Key, Dataset) ->
  maps:get(Key, Dataset).
update(Values, Dataset) ->
  pewpew_map_backed_data:update(Dataset, Values).
