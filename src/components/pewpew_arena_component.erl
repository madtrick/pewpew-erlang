-module(pewpew_arena_component).
-behaviour(gen_server).

-export([start_link/1]).
-export([
  get_player/2,
  players/1,
  positions_left/1,
  create_player/2,
  move_player/2,
  dimensions/1
]).
-export([init/1, handle_call/3, handle_info/2]).

-define(PLAYER_DOWN(Pid), {'DOWN', _, process, Pid, _}).
-define(COLORS, [<<"red">>, <<"blue">>, <<"green">>]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Data) ->
  gen_server:start_link(?MODULE, [Data], []).

positions_left(ArenaComponent) ->
  gen_server:call(ArenaComponent, positions_left).

create_player(ArenaComponent, Data) ->
  gen_server:call(ArenaComponent, {create_player, Data}).

get_player(ArenaComponent, Data) ->
  gen_server:call(ArenaComponent, {get_player, Data}).

move_player(ArenaComponent, Data) ->
  gen_server:call(ArenaComponent, {move_player, Data}).

dimensions(ArenaComponent) ->
  gen_server:call(ArenaComponent, dimensions).

players(ArenaComponent) ->
  gen_server:call(ArenaComponent, players).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Data]) ->
  {ok, PewpewPlayerComponentSup} = pewpew_player_component_sup:start_link(),
  {ok, pewpew_arena_component_data:new([
        {pewpew_player_component_sup, PewpewPlayerComponentSup},
        {pewpew_game_context_data, proplists:get_value(pewpew_game_context_data, Data)},
        {max_number_of_players, pewpew_config:get('arena.players.max')},
        {width, proplists:get_value(width, Data)},
        {height, proplists:get_value(height, Data)}
      ])}.

handle_info(?PLAYER_DOWN(Pid), ArenaComponentData) ->
  NewPlayers = [Player || Player <- pewpew_arena_component_data:players(ArenaComponentData), Player =/= Pid ],
  NewArenaComponentData = pewpew_arena_component_data:update(ArenaComponentData, [{players, NewPlayers}]),
  {noreply, NewArenaComponentData}.

handle_call({create_player, Data}, _, ArenaComponentData) ->
  Color        = pick_player_color(ArenaComponentData),
  Id           = pick_player_id(ArenaComponentData),
  {x, X, y, Y} = pick_player_coordinates(ArenaComponentData),
  Name         = pick_player_name(ArenaComponentData),
  Radius       = pick_player_radius(),

  {ok, Player}                = pewpew_player_component_sup:add_player(
    pewpew_arena_component_data:pewpew_player_component_sup(ArenaComponentData),
    pewpew_arena_component_data:pewpew_game_context_data(ArenaComponentData),
    [{color, Color}, {id, Id}, {x, X}, {y, Y}, {name, Name}, {radius, Radius} | Data]
  ),

  monitor_player_componet(Player),
  Players               = pewpew_arena_component_data:players(ArenaComponentData),
  NewArenaComponentData = pewpew_arena_component_data:update(ArenaComponentData, [{players, [Player | Players]}]),
  {reply, Player, NewArenaComponentData};

handle_call({get_player, PlayerId}, _, ArenaComponentData) ->
  {ok, Player} = pewpew_arena_component_mod:get_player(PlayerId, ArenaComponentData),
  {reply, Player, ArenaComponentData};

handle_call({move_player, PlayerId, Data}, _, ArenaComponentData) ->
  {ok, Player} = pewpew_arena_component_mod:get_player(PlayerId, ArenaComponentData),
  ok           = pewpew_arena_component_mod:move_player(Player, Data, ArenaComponentData),

  {reply, ok, ArenaComponentData};

handle_call(players, _, ArenaComponentData) ->
  {reply, pewpew_arena_component_data:players(ArenaComponentData), ArenaComponentData};

handle_call(positions_left, _, ArenaComponentData) ->
  {reply, real_positions_left(ArenaComponentData), ArenaComponentData};

handle_call(dimensions, _, ArenaComponentData) ->
  {ok, Dimensions} = pewpew_arena_component_mod:dimensions(ArenaComponentData),

  {reply, Dimensions, ArenaComponentData}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

real_positions_left(ArenaComponentData) ->
  pewpew_arena_component_data:max_number_of_players(ArenaComponentData) - number_of_players(ArenaComponentData).

monitor_player_componet(Player) ->
  erlang:monitor(process, Player).

number_of_players(ArenaComponentData) ->
  erlang:length(pewpew_arena_component_data:players(ArenaComponentData)).

pick_player_color(ArenaComponentData) ->
  Players       = pewpew_arena_component_data:players(ArenaComponentData),
  PlayersColors = [ pewpew_player_component:color(Player) || Player <- Players],
  [Color | _]   = available_colors(?COLORS, PlayersColors),
  Color.

available_colors(AllColors, UsedColors) ->
  lists:filter(fun(X) -> not lists:member(X, UsedColors) end, AllColors).

pick_player_id(_) ->
  fserlangutils_time:microseconds_since_epoch().

pick_player_coordinates(ArenaComponentData) ->
  %NOTE: for now I'm harcoding here the radius of the player
  % Substract the double of the radious and the add the radious
  % after pick_player_?_coordinate. This is because pick_player_?_coordinate
  % could return a value < radious.
  %
  % radius : 16

  X = pick_player_x_coordinate(pewpew_arena_component_data:width(ArenaComponentData) - 32) + 16,
  Y = pick_player_y_coordinate(pewpew_arena_component_data:height(ArenaComponentData) - 32) + 16,

  {x, X, y, Y}.

pick_player_x_coordinate(MaxX) ->
  random:uniform(MaxX).

pick_player_y_coordinate(MaxY) ->
  random:uniform(MaxY).

pick_player_name(_ArenaComponentData) ->
  <<"Player">>.

pick_player_radius() ->
  5. %in px
