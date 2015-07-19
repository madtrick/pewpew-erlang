-module(pewpew_radar_component).
-behaviour(gen_server).

-export([
  start_link/0,
  scan/3
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

scan(Radar, ScanContext, RadarConfig) ->
  gen_server:call(Radar, {scan, ScanContext, RadarConfig}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_) ->
  {ok, undefined}.

handle_call({scan, ScanContext, _RadarConfig}, _, Data) ->
  #{
    arena_dimensions := ArenaDimensions,
    players := Players,
    scanning_player := ScanningPlayer
  } = ScanContext,


  ScanResult = pewpew_radar_component_mod:circular_scan(ArenaDimensions, Players, ScanningPlayer, 40),
  #{
                                                                                    walls := Sw,
                                                                                    players := SP
                                                                                   } = ScanResult,
  ScanResultData = pewpew_radar_scan_result_data:new([{scanned_walls, Sw}, {scanned_players, SP}]),
  {reply, ScanResultData, Data}.

terminate(_, _) ->
  normal.
