-module(pewpew_radar_tests).
-include_lib("eunit/include/eunit.hrl").

detects_no_player_when_there_are_no_players_test_() ->
  generate_circular_radar_test(#{
    players => [
      [{id, player_id_1}, {x, 200}, {y, 200}]
    ],
    players_to_find => []
   }).

detects_player_with_center_under_radar_test_() ->
  generate_circular_radar_test(#{
    players => [
      [{id, player_id_1}, {x, 200}, {y, 200}],
      [{id, player_id_2}, {x, 220}, {y, 200}]
    ],
    players_to_find => [player_id_2]
   }).

does_not_detect_player_with_center_out_of_radar_test_() ->
  generate_circular_radar_test(#{
    players => [
      [{id, player_id_1}, {x, 100}, {y, 200}],
      [{id, player_id_2}, {x, 220}, {y, 200}]
    ],
    players_to_find => []
   }).

detect_right_wall_test_() ->
  generate_circular_radar_test(#{
    players => [[{id, player_id_1}, {x, 380}, {y, 200}]],
    walls_to_find => [[{400, 234}, {400, 165}]]
   }).

detect_right_tangent_wall_test_() ->
  generate_circular_radar_test(#{
    players => [[{id, player_id_1}, {x, 360}, {y, 200}]],
    walls_to_find => [[{400, 200}]]
   }).

detect_top_wall_test_() ->
  generate_circular_radar_test(#{
    players => [[{id, player_id_1}, {x, 200}, {y, 380}]],
    walls_to_find => [[{234, 400}, {165, 400}]]
   }).

detect_tangent_top_wall_test_() ->
  generate_circular_radar_test(#{
    players => [[{id, player_id_1}, {x, 200}, {y, 360}]],
    walls_to_find => [[{200, 400}]]
   }).

detect_left_wall_test_() ->
  generate_circular_radar_test(#{
    players => [[{id, player_id_1}, {x, 20}, {y, 200}]],
    walls_to_find => [[{0, 234}, {0, 165}]]
   }).

detect_left_tangent_wall_test_() ->
  generate_circular_radar_test(#{
    players => [[{id, player_id_1}, {x, 40}, {y, 200}]],
    walls_to_find => [[{0, 200}]]
   }).

detect_bottom_wall_test_() ->
  generate_circular_radar_test(#{
    players => [[{id, player_id_1}, {x, 200}, {y, 20}]],
    walls_to_find => [[{234, 0}, {165, 0}]]
   }).

detect_tangent_bottom_wall_test_() ->
  generate_circular_radar_test(#{
    players => [[{id, player_id_1}, {x, 200}, {y, 40}]],
    walls_to_find => [[{200, 0}]]
   }).

detect_right_top_corner_wall_test_() ->
  generate_circular_radar_test(#{
    players => [[{id, player_id_1}, {x, 380}, {y, 380}]],
    walls_to_find => [[{400,400},{345,400}],[{400,400},{400,345}]]
   }).

detect_left_top_corner_wall_test_() ->
  generate_circular_radar_test(#{
    players => [[{id, player_id_1}, {x, 20}, {y, 380}]],
    walls_to_find => [[{54,400},{0,400}],[{0,400},{0,345}]]
   }).

detect_right_bottom_corner_wall_test_() ->
  generate_circular_radar_test(#{
    players => [[{id, player_id_1}, {x, 380}, {y, 20}]],
    walls_to_find => [[{400,0},{345,0}],[{400,54},{400,0}]]
   }).

detect_left_bottom_corner_wall_test_() ->
  generate_circular_radar_test(#{
    players => [[{id, player_id_1}, {x, 20}, {y, 20}]],
    walls_to_find => [[{54,0},{0,0}],[{0,54},{0,0}]]
   }).

long_range_radar_player_o2_rotated_test_() ->
  generate_long_range_radar_test(#{
    players => [
      [{id, player_id_1}, {x, 200}, {y, 200}],
      [{id, player_id_2}, {x, 270}, {y, 210}]
    ],
    players_to_find => [player_id_2]
   }).

long_range_radar_player_o_rotated_test_() ->
  generate_long_range_radar_test(#{
    players => [
      [{id, player_id_1}, {x, 200}, {y, 200}],
      [{id, player_id_2}, {x, 220}, {y, 220}]
    ],
    players_to_find => []
   }).

long_range_radar_player__rotated_test_() ->
  generate_long_range_radar_test(#{
    players => [
      [{id, player_id_1}, {x, 200}, {y, 200}, {rotation, math:pi()}],
      [{id, player_id_2}, {x, 200}, {y, 160}]
    ],
    players_to_find => []
   }).

long_range_radar_player_rotated_test_() ->
  generate_long_range_radar_test(#{
    players => [
      [{id, player_id_1}, {x, 200}, {y, 200}, {rotation, math:pi()}],
      [{id, player_id_2}, {x, 200}, {y, 240}]
    ],
    players_to_find => [player_id_2]
   }).

long_range_radar_player_rotated_2_test_() ->
  generate_long_range_radar_test(#{
    players => [
      [{id, player_id_1}, {x, 200}, {y, 200}, {rotation, math:pi()}],
      [{id, player_id_2}, {x, 210}, {y, 240}]
    ],
    players_to_find => [player_id_2]
   }).

long_range_radar_player_rotated_3_test_() ->
  generate_long_range_radar_test(#{
    players => [
      [{id, player_id_1}, {x, 200}, {y, 200}, {rotation, math:pi()}],
      [{id, player_id_2}, {x, 210}, {y, 300}]
    ],
    players_to_find => []
   }).

long_range_radar_player_rotated_4_test_() ->
  generate_long_range_radar_test(#{
    players => [
      [{id, player_id_1}, {x, 200}, {y, 200}, {rotation, math:pi()}],
      [{id, player_id_2}, {x, 190}, {y, 260}]
    ],
    players_to_find => [player_id_2]
   }).

long_range_radar_player_rotated_5_test_() ->
  generate_long_range_radar_test(#{
    players => [
      [{id, player_id_1}, {x, 200}, {y, 200}, {rotation, math:pi()}],
      [{id, player_id_2}, {x, 136}, {y, 163}] % on the radar edge
    ],
    players_to_find => [player_id_2]
   }).

long_range_radar_player_rotated_6_test_() ->
  generate_long_range_radar_test(#{
    players => [
      [{id, player_id_1}, {x, 200}, {y, 200}, {rotation, math:pi()}],
      [{id, player_id_2}, {x, 136}, {y, 163}], % on the radar edge
      [{id, player_id_3}, {x, 190}, {y, 260}]
    ],
    players_to_find => [player_id_2, player_id_3]
   }).

generate_circular_radar_test(Setup) ->
  CircularRadarSetup = maps:merge(
    Setup,
    #{radar => scan, radius => 40}
  ),

  generate_radar_test(CircularRadarSetup).

generate_long_range_radar_test(Setup) ->
  LongRangeRadarSetup = maps:merge(
    Setup,
    #{radar => long_range_scan, radius => 80}
  ),

  generate_radar_test(LongRangeRadarSetup).

generate_radar_test(Setup) ->
  Defaults = #{
    players_to_find => [],
    walls_to_find => []
  },

  UpdatedSetup = maps:merge(Defaults, Setup),

  #{
    players := Players,
    players_to_find := PlayersToFind,
    walls_to_find := WallsToFind,
    radar := Radar,
    radius := Radius
  } = UpdatedSetup,

  {setup,
    fun() ->
        application:set_env(pewpew, execution_mode, test),
        pewpew_config:load(),
        Options = #{
          arena_options => [{width, 400}, {height, 400}, {pewpew_game_context_data, undefined}],
          players_options => Players
         },
        ArenaComponent = pewpew_arena_component_factory:create(Options),
        ArenaComponent
    end,
    fun(ArenaComponent) ->
        pewpew_arena_component:stop(ArenaComponent)
    end,
    fun(ArenaComponent) ->
        ScanningPlayer      = pewpew_arena_component:get_player(ArenaComponent, player_id_1),
        Scan                = pewpew_radar:Radar(ArenaComponent, ScanningPlayer, Radius),
        #{players := ScannedPlayers, walls := WallScan} = Scan,

        AllPlayersFound = lists:all(fun(PlayerFound) ->
          lists:any(fun(Id) -> Id =:= pewpew_player_component:id(PlayerFound) end, PlayersToFind)
        end, ScannedPlayers),

        [
         ?_assert(AllPlayersFound),
         ?_assertEqual(WallsToFind, WallScan)
        ]
    end
  }.

%generate_wall_test(PlayerCoordinates, Expectations) ->
%  {setup,
%    fun() ->
%        application:set_env(pewpew, execution_mode, test),
%        pewpew_config:load(),

%        PlayerOptions = lists:flatten([ PlayerCoordinates | [{id, player_id_1} | []] ]),
%        Options = #{
%          arena_options => [{width, 400}, {height, 400}, {pewpew_game_context_data, undefined}],
%          players_options => [PlayerOptions]
%         },
%        ArenaComponent = pewpew_arena_component_factory:create(Options),
%        ArenaComponent
%    end,
%    fun(ArenaComponent) ->
%        pewpew_arena_component:stop(ArenaComponent)
%    end,
%    fun(ArenaComponent) ->
%        ScanningPlayer = pewpew_arena_component:get_player(ArenaComponent, player_id_1),
%        Scan   = pewpew_radar:scan(ArenaComponent, ScanningPlayer, 40),

%        #{players := PlayersScan, walls := WallScan} = Scan,

%        [
%         ?_assertEqual([], PlayersScan),
%         ?_assertEqual(Expectations, WallScan)
%        ]
%    end
%  }.
