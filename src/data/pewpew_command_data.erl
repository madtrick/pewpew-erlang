-module(avioneta_command_data).

-export([new/2, runner/1, runner_data/1]).

-record(avioneta_command_data, {
    runner,
    runner_data
  }).

new(Runner, RunnerData) ->
  #avioneta_command_data{
    runner      = Runner,
    runner_data = RunnerData
  }.

runner(#avioneta_command_data{ runner = Runner}) -> Runner.
runner_data(#avioneta_command_data{ runner_data = RunnerData}) -> RunnerData.
