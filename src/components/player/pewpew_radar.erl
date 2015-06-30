-module(pewpew_radar).
-behaviour(gen_server).

-export([
  start_link/0,
  scan/1
  ]).

-export([
  init/1,
  handle_call/3,
  terminate/2
  ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
  gen_server:start_link(?MODULE, [], []).

scan(Radar) ->
  gen_server:call(Radar, scan).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_) ->
  Radius = 40,
  Mode   = circular_scan,
  Data   = pewpew_radar_data:new([{radar_mode, Mode}, {radar_radius, Radius}]),
  {ok, Data}.

handle_call(scan, _, Data) ->
  ScanResult = pewpew_radar_scan_result_data:new([{scanned_walls, []}, {scanned_players, []}]),
  {reply, ScanResult, Data}.

terminate(_, _) ->
  normal.
