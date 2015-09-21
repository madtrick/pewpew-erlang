-module(pewpew_player_component).
-behaviour(gen_server).

-export([start_link/2]).
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2
]).
-export([
  get_state/1,
  set_state/2,
  id/1,
  x/1,
  y/1,
  move/2,
  destroy/1,
  color/1,
  name/1,
  hit/1,
  life/1,
  rotate/2,
  rotation/1,
  channel/1,
  radius/1,
  coordinates/1,
  snapshot/1,
  update/2,
  radar_config/1,
  configure/3
]).

% Exported only for testing
-export([
  set_coordinates/2
]).

-define(PROCESS_DOWN(Pid), {'DOWN', _MonitorRef, process, Pid, _}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(PewpewGameContextData, PlayerData) ->
  gen_server:start_link(?MODULE, [PewpewGameContextData, PlayerData], []).

get_state(PlayerComponent) ->
  gen_server:call(PlayerComponent, get_state).

set_state(PlayerComponent, Data) ->
  gen_server:cast(PlayerComponent, {set_state, Data}).

id(PlayerComponent) ->
  gen_server:call(PlayerComponent, id).

x(PlayerComponent) ->
  {x, X, _, _} = coordinates(PlayerComponent),
  X.

y(PlayerComponent) ->
  {_, _, y, Y} = coordinates(PlayerComponent),
  Y.

hit(PlayerComponent) ->
  gen_server:cast(PlayerComponent, hit).

color(PlayerComponent) ->
  gen_server:call(PlayerComponent, color).

name(PlayerComponent) ->
  gen_server:call(PlayerComponent, name).

move(PlayerComponent, Data) ->
  gen_server:cast(PlayerComponent, {move, Data}).

destroy(PlayerComponent) ->
  gen_server:cast(PlayerComponent, destroy).

life(PlayerComponent) ->
  gen_server:call(PlayerComponent, life).

rotate(PlayerComponent, Data) ->
  gen_server:cast(PlayerComponent, {rotate, Data}).

rotation(PlayerComponent) ->
  gen_server:call(PlayerComponent, rotation).

channel(PlayerComponent) ->
  gen_server:call(PlayerComponent, channel).

radius(PlayerComponent) ->
  gen_server:call(PlayerComponent, radius).

set_coordinates(PlayerComponent, Coordinates) ->
  gen_server:cast(PlayerComponent, {set_coordinates, Coordinates}).

coordinates(PlayerComponent) ->
  gen_server:call(PlayerComponent, coordinates).

snapshot(PlayerComponent) ->
  gen_server:call(PlayerComponent, snapshot).

update(PlayerComponent, UpdateContext) ->
  gen_server:call(PlayerComponent, {update, UpdateContext}).

radar_config(PlayerComponent) ->
  gen_server:call(PlayerComponent, radar_config).

configure(PlayerComponent, Op, Args) ->
  gen_server:call(PlayerComponent, {configure, Op, Args}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callback
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([PewpewGameContextData, PlayerData]) ->
  %TODO: check if the player uses the game context data for anything
  %TODO: move the generation of the radar_config data to arena_component_mod
  PlayerRadarConfigData = pewpew_radar_config_data:new(
    [{mode, circular_scan}, {radius, 40}]
  ),
  PlayerComponentData = pewpew_player_component_data:new(
    [
     {radar_config_data, PlayerRadarConfigData},
     {pewpew_game_context_data, PewpewGameContextData}
     | PlayerData
    ]
  ),
  monitor_origin(PlayerComponentData),
  {ok, PlayerComponentData}.

handle_info(?PROCESS_DOWN(Pid), PlayerComponentData) ->
  react_to_process_down(PlayerComponentData, Pid).

handle_cast(destroy, PlayerComponentData) ->
  {stop, destroyed, PlayerComponentData};
handle_cast(hit, PlayerComponentData) ->
  {noreply, hit2(PlayerComponentData)};
handle_cast({move, Data}, PlayerComponentData) ->
  [{direction, Direction}]     = Data,
  {ok, NewPlayerComponentData} = pewpew_player_component_mod:move(Direction, PlayerComponentData),
  {noreply, NewPlayerComponentData};
handle_cast({rotate, Data}, PlayerComponentData) ->
  {noreply, pewpew_player_component_data:update(PlayerComponentData, [{rotation, Data}])};
handle_cast({set_coordinates, Coordinates}, PlayerComponentData) ->
  {ok, NewPlayerComponentData} = pewpew_player_component_mod:set_coordinates(PlayerComponentData, Coordinates),
  {noreply, NewPlayerComponentData};
handle_cast({set_state, Data}, _) ->
  {noreply, Data}.

handle_call({update, UpdateContext}, _, PlayerComponentData) ->
  {ok, UpdatedPlayerComponentData, Notifications} = pewpew_player_component_mod:update(PlayerComponentData, UpdateContext),
  {reply, {ok, Notifications}, UpdatedPlayerComponentData};
handle_call(get_state, _, PlayerComponentData) ->
  {reply, PlayerComponentData, PlayerComponentData};
handle_call(id, _, PlayerComponentData) ->
  {reply, pewpew_player_component_data:id(PlayerComponentData), PlayerComponentData};
handle_call(color, _, PlayerComponentData) ->
  {reply, pewpew_player_component_data:color(PlayerComponentData), PlayerComponentData};
handle_call(life, _, PlayerComponentData) ->
  {reply, player_component_data_life(PlayerComponentData), PlayerComponentData};
handle_call(name, _, PlayerComponentData) ->
  {reply, pewpew_player_component_data:name(PlayerComponentData), PlayerComponentData};
handle_call(rotation, _, PlayerComponentData) ->
  {reply, pewpew_player_component_data:rotation(PlayerComponentData), PlayerComponentData};
handle_call(channel, _, PlayerComponentData) ->
  {reply, pewpew_player_component_data:origin(PlayerComponentData), PlayerComponentData};
handle_call(radius, _, PlayerComponentData) ->
  {reply, pewpew_player_component_data:radius(PlayerComponentData), PlayerComponentData};
handle_call(coordinates, _, PlayerComponentData) ->
  {ok, Coordinates} = pewpew_player_component_mod:get_coordinates(PlayerComponentData),
  {reply, Coordinates, PlayerComponentData};
handle_call(snapshot, _, PlayerComponentData) ->
  {ok, PlayerState} = pewpew_player_component_mod:snapshot(PlayerComponentData),
  {reply, PlayerState, PlayerComponentData};
handle_call(radar_config, _, PlayerComponentData) ->
  RadarConfigData = pewpew_player_component_data:radar_config_data(PlayerComponentData),
  {reply, RadarConfigData, PlayerComponentData};
handle_call({configure, Op, Args}, _, PlayerComponentData) ->
  {OkOrError, UpdatedPlayerComponentData} = pewpew_player_component_mod:configure(PlayerComponentData, Op, Args),
  {reply, OkOrError, UpdatedPlayerComponentData}.

terminate(_Repos, _PlayerComponentData) ->
  die.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hit2(PlayerComponentData) ->
  pewpew_player_component_data:update(
    PlayerComponentData,
    [{life, player_component_data_life(PlayerComponentData) - 10}]
  ).

player_component_data_life(PlayerComponentData) ->
  pewpew_player_component_data:life(PlayerComponentData).

monitor_origin(PlayerComponentData) ->
  erlang:monitor(process, pewpew_player_component_data:origin(PlayerComponentData)).

react_to_process_down(PlayerComponentData, Pid) ->
  react_to_process_down_if_origin(PlayerComponentData, pewpew_player_component_data:origin(PlayerComponentData), Pid).

react_to_process_down_if_origin(PlayerComponentData, Origin, Pid) when Origin =:= Pid ->
  {stop, normal, PlayerComponentData};
react_to_process_down_if_origin(PlayerComponentData, _, _) ->
  {noreply, PlayerComponentData}.
