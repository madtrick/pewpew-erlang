-module(pewpew_shot_component).
-behaviour(gen_server).

-export([start_link/1]).
-export([
  coordinates/1
]).
-export([
  init/1,
  handle_call/3,
  terminate/2
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(ShotData) ->
  gen_server:start_link(?MODULE, [ShotData], []).

coordinates(ShotComponent) ->
  gen_server:call(ShotComponent, coordinates).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callback
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([ShotData]) ->
  ShotComponentData = pewpew_shot_component_data:new(ShotData),
  {ok, ShotComponentData}.

handle_call(coordinates, _, ShotComponentData) ->
  {ok, Coordinates} = pewpew_shot_component_mod:get_coordinates(ShotComponentData),

  {reply, Coordinates, ShotComponentData}.

terminate(_Repos, _ShotComponentData) ->
  die.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
