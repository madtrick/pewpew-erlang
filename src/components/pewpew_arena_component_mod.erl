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

% exported for testing
-export([
  update_shots/1
  ]).

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
  ShootingShotCost      = pewpew_config:get([players, shooting, cost]),
  ShootingInitialTokens = pewpew_config:get([players, shooting, initial_tokens]),
  NewTokensPerCycle     = pewpew_config:get([players, shooting, new_tokens_per_cycle]),
  MaxTokens             = pewpew_config:get([players, shooting, max_tokens]),
  Speed                 = pewpew_config:get([players, movement, speed]),
  ShootingInfo = #{
      cost => ShootingShotCost,
      tokens => ShootingInitialTokens,
      new_tokens_per_cycle => NewTokensPerCycle,
      max_tokens => MaxTokens
      },

  PlayerConfig      = [
      {color, Color},
      {hits, []},
      {id, Id},
      {x, X},
      {y, Y},
      {name, Name},
      {radius, Radius},
      {shooting_info, ShootingInfo},
      {speed, Speed}
      | PlayerData],

  PlayerConfig.

create_shot(ArenaComponentData, ShotData) ->
  ShotsSupervisor   = pewpew_arena_component_data:shot_component_sup(ArenaComponentData),
  Player            = proplists:get_value(player, ShotData),
  PlayerCoordinates = pewpew_player_component:coordinates(Player),
  PlayerRotation    = pewpew_player_component:rotation(Player),
  PlayerRadius      = pewpew_player_component:radius(Player),
  ShotSpeed         = pewpew_config:get([shots, movement, speed]),

  pewpew_player_component:shoot(Player),

  {x, X, y, Y} = pick_shot_coordinates(PlayerRotation, PlayerRadius, PlayerCoordinates),
  ShotId = pick_shot_id(),

  {ok, Shot} = pewpew_shot_component_sup:add_shot(ShotsSupervisor, [
        {rotation, PlayerRotation},
        {x, X},
        {y, Y},
        {id, ShotId},
        {speed, ShotSpeed} | ShotData]),
  {ok, Shot}.

move_player(Player, Movement, _) ->
  pewpew_player_component:move(Player, Movement),
  ok.

dimensions(ArenaComponentData) ->
  Width  = pewpew_arena_component_data:width(ArenaComponentData),
  Height = pewpew_arena_component_data:height(ArenaComponentData),

  {ok, {width, Width, height, Height}}.

update(ArenaComponentData) ->
  {UACD, N} = update_shots(ArenaComponentData),
  {UACD2, N2} = update_players(UACD),
  {UACD3, N3} = update_radars(UACD2),

  Updates = lists:append([N, N2, N3]),

  {ok, lists:flatten(Updates), UACD3}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_shots(ArenaComponentData) ->
  Shots = pewpew_arena_component_data:shots(ArenaComponentData),
  lists:foreach(fun pewpew_shot_component:move/1, Shots),

  Players = pewpew_arena_component_data:players(ArenaComponentData),
  Height = pewpew_arena_component_data:height(ArenaComponentData),
  Width = pewpew_arena_component_data:width(ArenaComponentData),
  UpdateContext = #{
    arena_dimensions => #{
        width => Width,
        height => Height
        },
    players => Players
  },

  {Updates, S} = lists:foldl(fun(Shot, {ShotUpdates, RemainingShots}) ->
    case pewpew_shot_component:update(Shot, UpdateContext) of
      updated ->
            {[do_nothing | ShotUpdates], [Shot | RemainingShots]};
      destroyed ->
            {[do_nothing | ShotUpdates], RemainingShots}
        end
    end, {[], []}, Shots),

  UpdatedArenaComponentData = pewpew_player_component_data:update(ArenaComponentData, [{shots, S}]),
  {UpdatedArenaComponentData, Updates}.

update_players(ArenaComponentData) ->
  Players = pewpew_arena_component_data:players(ArenaComponentData),
  update_players(ArenaComponentData, Players, [], []).

update_players(ArenaComponentData, [], NonDestroyedPlayers, PlayersNotifications) ->
  UpdatedArenaComponentData = pewpew_arena_component_data:update(ArenaComponentData, [{players, NonDestroyedPlayers}]),
  {UpdatedArenaComponentData, PlayersNotifications};
update_players(ArenaComponentData, [Player | Players], NonDestroyedPlayers, PlayersNotifications) ->
  ?debugVal(pewpew_arena_component_data:shots(ArenaComponentData)),
  ShotsContext = lists:map(fun(Shot) ->
          ?debugVal(Shot),
    {x, X, y, Y} = pewpew_shot_component:coordinates(Shot),
    {x, X, y, Y, radius, 1}
  end, pewpew_arena_component_data:shots(ArenaComponentData)),
  UpdateContext = #{shots => ShotsContext},

  case pewpew_player_component:update(Player, UpdateContext) of
    {updated, PlayerUpdateNotifications} ->
      update_players(ArenaComponentData, Players, [Player | NonDestroyedPlayers], [PlayerUpdateNotifications | PlayersNotifications]);
    {destroyed, PlayerUpdateNotifications} ->
      update_players(ArenaComponentData, Players, NonDestroyedPlayers, [PlayerUpdateNotifications | PlayersNotifications])
  end.

update_radars(ArenaComponentData) ->
  Players = pewpew_arena_component_data:players(ArenaComponentData),
  update_radar(Players, Players, ArenaComponentData, []).

update_radar([], _, ArenaComponentData, Notifications) ->
  {ArenaComponentData, Notifications};
update_radar([Player | Players], AllPlayers, ArenaComponentData, Notifications) ->
  RadarComponent  = pewpew_arena_component_data:radar_component(ArenaComponentData),
  Height          = pewpew_arena_component_data:height(ArenaComponentData),
  Width           = pewpew_arena_component_data:width(ArenaComponentData),
  RadarConfig     = pewpew_player_component:radar_config(Player),
  Channel         = pewpew_player_component:channel(Player),
  ArenaDimensions = {width, Width, height, Height},

  ScanContext = #{
    arena_dimensions => ArenaDimensions,
    players => AllPlayers,
    scanning_player => Player
   },

  ScanResult       = pewpew_radar_component:scan(RadarComponent, ScanContext, RadarConfig),
  ScanNotification = radar_scan_to_notification(ScanResult),
  RadarUpdate      = {player, Channel, update, ScanNotification},

  update_radar(Players, AllPlayers, ArenaComponentData, [RadarUpdate | Notifications]).

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

pick_player_color(_ArenaComponentData) ->
  pewpew_utils:hex_value(8).

pick_shot_id() ->
  pewpew_utils:get_current_time_in_milliseconds().
