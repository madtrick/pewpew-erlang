-module(pewpew_command_data).

-export([new/2, runner/1, runner_data/1]).

-record(pewpew_command_data, {
    runner,
    runner_data
  }).

new(Runner, RunnerData) ->
  #pewpew_command_data{
    runner      = Runner,
    runner_data = RunnerData
  }.

runner(#pewpew_command_data{ runner = Runner}) -> Runner.
runner_data(#pewpew_command_data{ runner_data = RunnerData}) -> RunnerData.
