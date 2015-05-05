-module(avioneta_arena_component).
-behaviour(gen_server).

-export([start_link/1]).
-export([get_player/2, players/1, positions_left/1, create_player/2]).
-export([init/1, handle_call/3, handle_info/2]).

-define(PLAYER_DOWN(Pid), {'DOWN', _, process, Pid, _}).
-define(COLORS, [<<"red">>, <<"blue">>, <<"green">>]).

start_link(Data) ->
  gen_server:start_link(?MODULE, [Data], []).

positions_left(ArenaComponent) ->
  gen_server:call(ArenaComponent, positions_left).

create_player(ArenaComponent, Data) ->
  gen_server:call(ArenaComponent, {create_player, Data}).

get_player(ArenaComponent, Data) ->
  gen_server:call(ArenaComponent, {get_player, Data}).

players(ArenaComponent) ->
  gen_server:call(ArenaComponent, players).

init([Data]) ->
  {ok, AvionetaPlayerComponentSup} = avioneta_player_component_sup:start_link(),
  {ok, avioneta_arena_component_data:new([
        {avioneta_player_component_sup, AvionetaPlayerComponentSup},
        {avioneta_game_context_data, proplists:get_value(avioneta_game_context_data, Data)},
        {max_number_of_players, avioneta_config:get('arena.players.max')},
        {width, proplists:get_value(width, Data)},
        {height, proplists:get_value(height, Data)}
      ])}.

handle_info(?PLAYER_DOWN(Pid), ArenaComponentData) ->
  NewPlayers = [Player || Player <- avioneta_arena_component_data:players(ArenaComponentData), Player =/= Pid ],
  NewArenaComponentData = avioneta_arena_component_data:update(ArenaComponentData, [{players, NewPlayers}]),
  {noreply, NewArenaComponentData}.

handle_call({create_player, Data}, _, ArenaComponentData) ->
  Color        = pick_player_color(ArenaComponentData),
  Id           = pick_player_id(ArenaComponentData),
  {x, X, y, Y} = pick_player_coordinates(ArenaComponentData),
  Name         = pick_player_name(ArenaComponentData),

  {ok, Player}                = avioneta_player_component_sup:add_player(
    avioneta_arena_component_data:avioneta_player_component_sup(ArenaComponentData),
    avioneta_arena_component_data:avioneta_game_context_data(ArenaComponentData),
    [{color, Color}, {id, Id}, {x, X}, {y, Y}, {name, Name} | Data]
  ),

  monitor_player_componet(Player),
  Players               = avioneta_arena_component_data:players(ArenaComponentData),
  NewArenaComponentData = avioneta_arena_component_data:update(ArenaComponentData, [{players, [Player | Players]}]),
  {reply, Player, NewArenaComponentData};

handle_call({get_player, Data}, _, ArenaComponentData) ->
  PlayerId = Data,
  [Player] = [Player || Player <- avioneta_arena_component_data:players(ArenaComponentData), avioneta_player_component:id(Player) =:= PlayerId],
  {reply, Player, ArenaComponentData};

handle_call(players, _, ArenaComponentData) ->
  {reply, avioneta_arena_component_data:players(ArenaComponentData), ArenaComponentData};

handle_call(positions_left, _, ArenaComponentData) ->
  {reply, real_positions_left(ArenaComponentData), ArenaComponentData}.

real_positions_left(ArenaComponentData) ->
  avioneta_arena_component_data:max_number_of_players(ArenaComponentData) - number_of_players(ArenaComponentData).

monitor_player_componet(Player) ->
  erlang:monitor(process, Player).

number_of_players(ArenaComponentData) ->
  erlang:length(avioneta_arena_component_data:players(ArenaComponentData)).

pick_player_color(ArenaComponentData) ->
  Players       = avioneta_arena_component_data:players(ArenaComponentData),
  PlayersColors = [ avioneta_player_component:color(Player) || Player <- Players],
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

  X = pick_player_x_coordinate(avioneta_arena_component_data:width(ArenaComponentData) - 32) + 16,
  Y = pick_player_y_coordinate(avioneta_arena_component_data:height(ArenaComponentData) - 32) + 16,

  {x, X, y, Y}.

pick_player_x_coordinate(MaxX) ->
  random:uniform(MaxX).

pick_player_y_coordinate(MaxY) ->
  random:uniform(MaxY).

pick_player_name(_ArenaComponentData) ->
  <<"Player">>.
