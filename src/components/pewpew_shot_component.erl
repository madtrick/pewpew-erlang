-module(pewpew_shot_component).
-behaviour(gen_server).

-export([start_link/1]).
-export([
  init/1,
  terminate/2
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(ShotData) ->
  gen_server:start_link(?MODULE, [ShotData], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callback
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([ShotData]) ->
  ShotComponentData = pewpew_shot_component_data:new(ShotData),
  {ok, ShotComponentData}.

terminate(_Repos, _ShotComponentData) ->
  die.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
