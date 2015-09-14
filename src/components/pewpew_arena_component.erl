-module(pewpew_arena_component).
-behaviour(gen_server).

-export([start_link/1, stop/1]).

-export([
  get_player/2,
  players/1,
  shots/1,
  positions_left/1,
  create_player/2,
  create_shot/2,
  move_player/2,
  dimensions/1,
  snapshot/1,
  update/1
]).

-export([
  init/1,
  handle_call/3,
  handle_info/2,
  handle_cast/2,
  terminate/2
]).

-define(PLAYER_DOWN(Pid), {'DOWN', _, process, Pid, _}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Data) ->
  gen_server:start_link(?MODULE, [Data], []).

stop(ArenaComponent) ->
  gen_server:cast(ArenaComponent, stop).

positions_left(ArenaComponent) ->
  gen_server:call(ArenaComponent, positions_left).

create_player(ArenaComponent, Data) ->
  gen_server:call(ArenaComponent, {create_player, Data}).

create_shot(ArenaComponent, Data) ->
  gen_server:call(ArenaComponent, {create_shot, Data}).

get_player(ArenaComponent, Data) ->
  gen_server:call(ArenaComponent, {get_player, Data}).

move_player(ArenaComponent, Data) ->
  gen_server:call(ArenaComponent, {move_player, Data}).

dimensions(ArenaComponent) ->
  gen_server:call(ArenaComponent, dimensions).

players(ArenaComponent) ->
  gen_server:call(ArenaComponent, players).

shots(ArenaComponent) ->
  gen_server:call(ArenaComponent, shots).

snapshot(ArenaComponent) ->
  gen_server:call(ArenaComponent, snapshot).

update(ArenaComponent) ->
  gen_server:call(ArenaComponent, update).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Data]) ->
  {ok, PewpewPlayerComponentSup} = pewpew_player_component_sup:start_link(),
  {ok, PewpewShotComponentSup} = pewpew_shot_component_sup:start_link(),
  {ok, PewPewRadarComponent} = pewpew_radar_component:start_link(),
  {ok, pewpew_arena_component_data:new([
        {radar_component, PewPewRadarComponent},
        {pewpew_player_component_sup, PewpewPlayerComponentSup},
        {shot_component_sup, PewpewShotComponentSup},
        {pewpew_game_context_data, proplists:get_value(pewpew_game_context_data, Data)},
        {max_number_of_players, pewpew_config:get([arena, players, max])},
        {width, proplists:get_value(width, Data)},
        {height, proplists:get_value(height, Data)}
      ])}.

handle_info(?PLAYER_DOWN(Pid), ArenaComponentData) ->
  NewPlayers = [Player || Player <- pewpew_arena_component_data:players(ArenaComponentData), Player =/= Pid ],
  NewArenaComponentData = pewpew_arena_component_data:update(ArenaComponentData, [{players, NewPlayers}]),
  {noreply, NewArenaComponentData}.

handle_call(update, _, ArenaComponentData) ->
  {ok, Updates, UpdatedArenaComponentData} = pewpew_arena_component_mod:update(ArenaComponentData),
  {reply, Updates, UpdatedArenaComponentData};
handle_call(snapshot, _, ArenaComponentData) ->
  Snapshot = pewpew_arena_component_snapshot:new(ArenaComponentData),
  {reply, Snapshot, ArenaComponentData};
handle_call({create_player, Data}, _, ArenaComponentData) ->
  {ok, Player} = pewpew_arena_component_mod:create_player(ArenaComponentData, Data),

  Players               = pewpew_arena_component_data:players(ArenaComponentData),
  UpdatedPlayersList    = [{players, [Player | Players]}],
  NewArenaComponentData = pewpew_arena_component_data:update(ArenaComponentData, UpdatedPlayersList),

  {reply, Player, NewArenaComponentData};
handle_call({create_shot, Data}, _, ArenaComponentData) ->
  {ok, Shot} = pewpew_arena_component_mod:create_shot(ArenaComponentData, Data),

  Shots                 = pewpew_arena_component_data:shots(ArenaComponentData),
  UpdatedShotsList      = [{shots, [Shot | Shots]}],
  NewArenaComponentData = pewpew_arena_component_data:update(ArenaComponentData, UpdatedShotsList),

  {reply, {ok, Shot}, NewArenaComponentData};

handle_call({get_player, PlayerId}, _, ArenaComponentData) ->
  {ok, Player} = pewpew_arena_component_mod:get_player(PlayerId, ArenaComponentData),
  {reply, Player, ArenaComponentData};

handle_call({move_player, PlayerId, Data}, _, ArenaComponentData) ->
  {ok, Player} = pewpew_arena_component_mod:get_player(PlayerId, ArenaComponentData),
  ok           = pewpew_arena_component_mod:move_player(Player, Data, ArenaComponentData),

  {reply, ok, ArenaComponentData};

handle_call(players, _, ArenaComponentData) ->
  {reply, pewpew_arena_component_data:players(ArenaComponentData), ArenaComponentData};

handle_call(shots, _, ArenaComponentData) ->
  {reply, pewpew_arena_component_data:shots(ArenaComponentData), ArenaComponentData};

handle_call(positions_left, _, ArenaComponentData) ->
  {reply, real_positions_left(ArenaComponentData), ArenaComponentData};

handle_call(dimensions, _, ArenaComponentData) ->
  {ok, Dimensions} = pewpew_arena_component_mod:dimensions(ArenaComponentData),

  {reply, Dimensions, ArenaComponentData}.

handle_cast(stop, ArenaComponentData) ->
  {stop, normal, ArenaComponentData}.

terminate(_, _) ->
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

real_positions_left(ArenaComponentData) ->
  pewpew_arena_component_data:max_number_of_players(ArenaComponentData) - number_of_players(ArenaComponentData).

number_of_players(ArenaComponentData) ->
  erlang:length(pewpew_arena_component_data:players(ArenaComponentData)).
