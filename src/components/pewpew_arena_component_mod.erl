-module(pewpew_arena_component_mod).
-include_lib("eunit/include/eunit.hrl").

-export([
  get_player/2,
  create_player/2,
  create_shot/2,
  move_player/3,
  dimensions/1,
  update/1
]).

-define(COLORS, [<<"red">>, <<"blue">>, <<"green">>]).

get_player(Condition, ArenaComponentData) ->
  MatchingPlayers = find_players_matching_condition(Condition, ArenaComponentData),

  case MatchingPlayers of
    []       -> {ok, undefined};
    [Player] -> {ok, Player}
  end.

create_player(ArenaComponentData, PlayerData) ->
  Color        = pick_player_color(ArenaComponentData),
  Id           = pick_player_id(ArenaComponentData),
  {x, X, y, Y} = pick_player_coordinates(ArenaComponentData),
  Name         = pick_player_name(ArenaComponentData),
  Radius       = pick_player_radius(),

  PlayersSupervisor = pewpew_arena_component_data:pewpew_player_component_sup(ArenaComponentData),
  GameContextData   = pewpew_arena_component_data:pewpew_game_context_data(ArenaComponentData),
  PlayerConfig      = [{color, Color}, {id, Id}, {x, X}, {y, Y}, {name, Name}, {radius, Radius} | PlayerData],

  {ok, Player}      = pewpew_player_component_sup:add_player(
    PlayersSupervisor,
    GameContextData,
    PlayerConfig
  ),

  monitor_player_componet(Player),

  {ok, Player}.

create_shot(ArenaComponentData, ShotData) ->
  ShotsSupervisor   = pewpew_arena_component_data:shot_component_sup(ArenaComponentData),
  Player            = proplists:get_value(player, ShotData),
  PlayerCoordinates = pewpew_player_component:coordinates(Player),
  PlayerRotation    = pewpew_player_component:rotation(Player),
  PlayerRadius      = pewpew_player_component:radius(Player),

  {x, X, y, Y} = pick_shot_coordinates(PlayerRotation, PlayerRadius, PlayerCoordinates),

  {ok, Shot} = pewpew_shot_component_sup:add_shot(ShotsSupervisor, [{rotation, PlayerRotation}, {x, X}, {y, Y} | ShotData]),
  {ok, Shot}.

move_player(Player, Movement, _) ->
  pewpew_player_component:move(Player, Movement),
  ok.

dimensions(ArenaComponentData) ->
  Width  = pewpew_arena_component_data:width(ArenaComponentData),
  Height = pewpew_arena_component_data:height(ArenaComponentData),

  {ok, {width, Width, height, Height}}.

update(ArenaComponentData) ->
  Players        = pewpew_arena_component_data:players(ArenaComponentData),
  RadarComponent = pewpew_arena_component_data:radar_component(ArenaComponentData),
  Height = pewpew_arena_component_data:height(ArenaComponentData),
  Width = pewpew_arena_component_data:width(ArenaComponentData),
  ArenaDimensions = {width, Width, height, Height},

  PlayersUpdates = lists:map(fun(Player) ->
    RadarConfig      = pewpew_player_component:radar_config(Player),
    ScanContext = #{
      arena_dimensions => ArenaDimensions,
      players => Players,
      scanning_player => Player
     },
    ScanResult       = pewpew_radar_component:scan(RadarComponent, ScanContext, RadarConfig),
    ScanNotification = radar_scan_to_notification(ScanResult),
    RadarUpdate      = {player, Player, update, ScanNotification},
    %PlayerUpdate = {player, Player, update, pewpew_player_component:update(Player)},

    RadarUpdate
  end, Players),

  Shots = pewpew_arena_component_data:shots(ArenaComponentData),
  ShotUpdateContext = #{
    arena_dimensions => #{width => Width, height => Height}
  },
  {UACD, ShotsUpdates2} = lists:foldl(fun(Shot, {ACD, ShotUpdates}) ->

    {Action, NewACD} = case pewpew_shot_component:update(Shot, ShotUpdateContext) of
      updated ->
        {do_nothing, ACD};
      destroyed ->
        UpdatedShotsList = lists:filter(fun(S) -> S =/= Shot  end, Shots),
        UpdatedArenaComponentData = pewpew_arena_component_data:update(ACD, [{shots, UpdatedShotsList}]),
        {do_nothing, UpdatedArenaComponentData}
     end,

    {NewACD, [Action | ShotUpdates]}
  end, {ArenaComponentData, []}, Shots),

  Updates = [PlayersUpdates, ShotsUpdates2],

  {ok, lists:flatten(Updates), UACD}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

radar_scan_to_notification(ScanResult) ->
  ScanNotification = pewpew_radar_scan_notification:new(ScanResult),
  {notification, ScanNotification}.

find_players_matching_condition(Condition, ArenaComponentData) ->
  [
   Player ||
   Player <- pewpew_arena_component_data:players(ArenaComponentData),
   (pewpew_player_component:id(Player) =:= Condition) or
   (pewpew_player_component:channel(Player) =:= Condition)
  ].

pick_player_id(_) ->
  fserlangutils_time:microseconds_since_epoch().

pick_player_coordinates(ArenaComponentData) ->
  %NOTE: for now I'm harcoding here the radius of the player
  % Substract the double of the radious and the add the radious
  % after pick_player_?_coordinate. This is because pick_player_?_coordinate
  % could return a value < radious.
  %
  % radius : 16

  ArenaWidth  = pewpew_arena_component_data:width(ArenaComponentData),
  ArenaHeight = pewpew_arena_component_data:height(ArenaComponentData),

  X = pick_player_x_coordinate(ArenaWidth - 32) + 16,
  Y = pick_player_y_coordinate(ArenaHeight - 32) + 16,

  {x, X, y, Y}.

pick_shot_coordinates(PlayerRotation, PlayerRadius, {x, PlayerX, y, PlayerY}) ->
  % Rotate from players origin to
  % get the origin of the shot

  Radians = math:pi() * PlayerRotation / 180,
  X = math:cos(Radians) * (PlayerRadius),
  Y = math:sin(Radians) * (PlayerRadius),

  % Translate points with relation to arena origin
  % using the players coordinates
  TranslatedX = X + PlayerX,
  TranslatedY = Y + PlayerY,

  {x, TranslatedX, y, TranslatedY}.

pick_player_x_coordinate(MaxX) ->
  random:uniform(MaxX).

pick_player_y_coordinate(MaxY) ->
  random:uniform(MaxY).

pick_player_name(_ArenaComponentData) ->
  <<"Player">>.

pick_player_radius() ->
  5. %in px

pick_player_color(ArenaComponentData) ->
  Players       = pewpew_arena_component_data:players(ArenaComponentData),
  PlayersColors = [ pewpew_player_component:color(Player) || Player <- Players],
  [Color | _]   = available_colors(?COLORS, PlayersColors),
  Color.

available_colors(AllColors, UsedColors) ->
  lists:filter(fun(X) -> not lists:member(X, UsedColors) end, AllColors).

monitor_player_componet(Player) ->
  erlang:monitor(process, Player).
