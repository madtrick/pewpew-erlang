-module(pewpew_shot_component).
-behaviour(gen_server).
-include_lib("eunit/include/eunit.hrl").

-export([start_link/1]).
-export([
  coordinates/1,
  rotation/1,
  update/2,
  move/1
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

rotation(ShotComponent) ->
  gen_server:call(ShotComponent, rotation).

update(ShotComponent, UpdateContext) ->
  gen_server:call(ShotComponent, {update, UpdateContext}).

move(ShotComponent) ->
  gen_server:call(ShotComponent, move).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callback
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([ShotData]) ->
  ShotComponentData = pewpew_shot_component_data:new(ShotData),
  {ok, ShotComponentData}.

handle_call(move, _, ShotComponentData) ->
  {ok, UpdatedShotComponentData} = pewpew_shot_component_mod:move(ShotComponentData),
  {reply, ok, UpdatedShotComponentData};
handle_call({update, UpdateContext}, _, ShotComponentData) ->
  case pewpew_shot_component_mod:update(ShotComponentData, UpdateContext) of
    {ok, UpdatedShotComponentData} ->
      {reply, updated, UpdatedShotComponentData};
    {destroy, UpdatedShotComponentData} ->
      {stop, normal, destroyed, UpdatedShotComponentData}
  end;
handle_call(rotation, _, ShotComponentData) ->
  {reply, pewpew_shot_component_data:rotation(ShotComponentData), ShotComponentData};
handle_call(coordinates, _, ShotComponentData) ->
  {ok, Coordinates} = pewpew_shot_component_mod:get_coordinates(ShotComponentData),

  {reply, Coordinates, ShotComponentData}.

terminate(_Repos, _ShotComponentData) ->
  die.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
