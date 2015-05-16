-module(pewpew_map_backed_data).

-export([
  new/1,
  update/2
]).

new(Options) when is_list(Options) ->
  lists:foldl(fun(Option, Acc) ->
    {Key, Value} = Option,
    maps:put(Key, Value, Acc)
  end, #{}, Options).

update(Data, Proplist) when is_list(Proplist) ->
  MapKeys  = maps:keys(Data),
  DataKeys = proplists:get_keys(Proplist),
  Keys     = lists:usort(lists:flatten([MapKeys | DataKeys])),


  lists:foldl(fun(Key, Acc) ->
    Default          = maps:get(Key, Acc),
    MaybeOptionValue = proplists:get_value(Key, Proplist, Default),

    maps:update(Key, MaybeOptionValue, Acc)
  end, Data, Keys).
