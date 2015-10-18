-module(pewpew_shot_component).
-behaviour(gen_server).
-include_lib("eunit/include/eunit.hrl").

-export([start_link/1]).
-export([
  coordinates/1,
  rotation/1,
  update/2,
  move/1,
  x/1,
  y/1,
  id/1,
  snapshot/1
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

x(ShotComponent) ->
  {x, X, y, _} =gen_server:call(ShotComponent, coordinates),
  X.

y(ShotComponent) ->
  {x, _, y, Y} =gen_server:call(ShotComponent, coordinates),
  Y.

id(ShotComponent) ->
  gen_server:call(ShotComponent, id).

snapshot(ShotComponent) ->
  gen_server:call(ShotComponent, snapshot).

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
  {reply, Coordinates, ShotComponentData};
handle_call(snapshot, _, ShotComponentData) ->
  {ok, ShotSnapshot} = pewpew_shot_component_mod:snapshot(ShotComponentData),
  {reply, ShotSnapshot, ShotComponentData};
handle_call(id, _, ShotComponentData) ->
  {reply, pewpew_shot_component_data:id(ShotComponentData), ShotComponentData}.

terminate(_Repos, _ShotComponentData) ->
  die.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
