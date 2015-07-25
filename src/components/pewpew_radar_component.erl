-module(pewpew_radar_component).
-behaviour(gen_server).
-include("utils.hrl").
-include_lib("eunit/include/eunit.hrl").

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

scan(Radar, ScanContext, RadarConfigData) ->
  gen_server:call(Radar, {scan, ScanContext, RadarConfigData}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_) ->
  {ok, undefined}.

handle_call({scan, ScanContext, RadarConfigData}, _, Data) ->
  #{
    arena_dimensions := ArenaDimensions,
    players := Players,
    scanning_player := ScanningPlayer
  } = ScanContext,

  ScanMode   = ?b2a(pewpew_radar_config_data:mode(RadarConfigData)),
  ScanResult = pewpew_radar_component_mod:ScanMode(ArenaDimensions, Players, ScanningPlayer),

  #{ walls := ScannedWalls, players := ScannedPlayers } = ScanResult,
  ScanResultData = pewpew_radar_scan_result_data:new([{scanned_walls, ScannedWalls}, {scanned_players, ScannedPlayers}]),
  {reply, ScanResultData, Data}.

terminate(_, _) ->
  normal.
