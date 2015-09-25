-module(pewpew_test_runner).

-export([run/1]).

run(Tests) ->
  runner(filter(group(unify(Tests)))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
filter(Tests) ->
  filter(Tests, []).

filter([], Acc) ->
  lists:reverse(Acc);
filter([{Description, [focus] = Options, Test} | _], _) ->
  filter([], [{Description, Options, Test}]);
filter([{_, [], _} = T | Tests], Acc) ->
  filter(Tests, [T | Acc]).

runner(Tests) ->
  {inorder, runner(Tests, [])}.

runner([], Acc) ->
  Acc;
runner([{Title, _Options, Test} | Tests], Acc) ->
  runner(Tests, [{Title, Test} | Acc]).

group(Tests) ->
  group(Tests, []).

group([], Acc) ->
  lists:reverse(Acc);
group([{_, [focus], NestedTests} | _], _) when is_list(NestedTests) ->
  group(unify(NestedTests), []);
group([{_, _, NestedTests} | Tests], Acc) when is_list(NestedTests) ->
  group(Tests, lists:append([Acc, group(unify(NestedTests), [])]));
group([{Description, Options, Test} | Tests], Acc) when is_function(Test) ->
  group(Tests, [{Description, Options, Test()} | Acc]);
group([{_, _, Test} = T | Tests], Acc) when is_tuple(Test) ->
  group(Tests, [T | Acc]).

unify(Tests) ->
  unify(Tests, []).

unify([], Acc) ->
  lists:reverse(Acc);
unify([{Description, TestOrTests} | Tests], Acc) ->
  unify(Tests, [{Description, [], TestOrTests} | Acc ]);
unify([{Description, Option, TestOrTests} | Tests], Acc) when is_atom(Option) ->
  unify(Tests, [{Description, [Option], TestOrTests} | Acc]).
