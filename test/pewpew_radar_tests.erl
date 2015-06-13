-module(pewpew_radar_tests).
-include_lib("eunit/include/eunit.hrl").

detects_no_player_when_there_are_no_players_test_() ->
  {setup,
    fun() ->
        application:set_env(pewpew, execution_mode, test),
        pewpew_config:load(),

        PlayersOptions = [[{id, player_id_1}, {x, 200}, {y, 200}]],
        Options = #{
          arena_options => [{width, 400}, {height, 400}, {pewpew_game_context_data, undefined}],
          players_options => PlayersOptions
         },
        ArenaComponent = pewpew_arena_component_factory:create(Options),
        ArenaComponent
    end,
    fun(ArenaComponent) ->
        pewpew_arena_component:stop(ArenaComponent)
    end,
    fun(ArenaComponent) ->
        Player = pewpew_arena_component:get_player(ArenaComponent, player_id_1),
        Scan   = pewpew_radar:scan(ArenaComponent, Player, 40),
        #{players := PlayersScan, walls := WallScan} = Scan,

        [
         ?_assertEqual([], PlayersScan),
         ?_assertEqual([], WallScan)
        ]
    end
  }.

detects_player_with_center_under_radar_test_() ->
  {setup,
    fun() ->
        application:set_env(pewpew, execution_mode, test),
        pewpew_config:load(),

        PlayersOptions = [
          [{id, player_id_1}, {x, 200}, {y, 200}],
          [{id, player_id_2}, {x, 220}, {y, 200}]
        ],
        Options = #{
          arena_options => [{width, 400}, {height, 400}, {pewpew_game_context_data, undefined}],
          players_options => PlayersOptions
         },
        ArenaComponent = pewpew_arena_component_factory:create(Options),
        ArenaComponent
    end,
    fun(ArenaComponent) ->
        pewpew_arena_component:stop(ArenaComponent)
    end,
    fun(ArenaComponent) ->
        ScanningPlayer = pewpew_arena_component:get_player(ArenaComponent, player_id_1),
        DetectedPlayer = pewpew_arena_component:get_player(ArenaComponent, player_id_2),
        Scan   = pewpew_radar:scan(ArenaComponent, ScanningPlayer, 40),

        #{players := PlayersScan, walls := WallScan} = Scan,

        [
         ?_assertEqual([DetectedPlayer], PlayersScan),
         ?_assertEqual([], WallScan)
        ]
    end
  }.

does_not_detect_player_with_center_out_of_radar_test_() ->
  {setup,
    fun() ->
        application:set_env(pewpew, execution_mode, test),
        pewpew_config:load(),

        PlayersOptions = [
          [{id, player_id_1}, {x, 100}, {y, 200}],
          [{id, player_id_2}, {x, 220}, {y, 200}]
        ],
        Options = #{
          arena_options => [{width, 400}, {height, 400}, {pewpew_game_context_data, undefined}],
          players_options => PlayersOptions
         },
        ArenaComponent = pewpew_arena_component_factory:create(Options),
        ArenaComponent
    end,
    fun(ArenaComponent) ->
        pewpew_arena_component:stop(ArenaComponent)
    end,
    fun(ArenaComponent) ->
        ScanningPlayer = pewpew_arena_component:get_player(ArenaComponent, player_id_1),
        Scan   = pewpew_radar:scan(ArenaComponent, ScanningPlayer, 40),

        #{players := PlayersScan, walls := WallScan} = Scan,

        [
         ?_assertEqual([], PlayersScan),
         ?_assertEqual([], WallScan)
        ]
    end
  }.

detect_right_wall_test_() ->
  generate_wall_test(
    [{x, 380}, {y, 200}],
    [{400, 234}, {400, 165}]
   ).

detect_right_tangent_wall_test_() ->
  generate_wall_test(
    [{x, 360}, {y, 200}],
    [{400, 200}]
   ).

detect_top_wall_test_() ->
  generate_wall_test(
    [{x, 200}, {y, 380}],
    [{234, 400}, {165, 400}]
   ).

detect_tangent_top_wall_test_() ->
  generate_wall_test(
    [{x, 200}, {y, 360}],
    [{200, 400}]
   ).

detect_left_wall_test_() ->
  generate_wall_test(
    [{x, 20}, {y, 200}],
    [{0, 234}, {0, 165}]
   ).

detect_left_tangent_wall_test_() ->
  generate_wall_test(
    [{x, 40}, {y, 200}],
    [{0, 200}]
   ).


detect_bottom_wall_test_() ->
  generate_wall_test(
    [{x, 200}, {y, 20}],
    [{234, 0}, {165, 0}]
   ).

detect_tangent_bottom_wall_test_() ->
  generate_wall_test(
    [{x, 200}, {y, 40}],
    [{200, 0}]
   ).

generate_wall_test(PlayerCoordinates, Expectations) ->
  {setup,
    fun() ->
        application:set_env(pewpew, execution_mode, test),
        pewpew_config:load(),

        PlayerOptions = lists:flatten([ PlayerCoordinates | [{id, player_id_1} | []] ]),
        Options = #{
          arena_options => [{width, 400}, {height, 400}, {pewpew_game_context_data, undefined}],
          players_options => [PlayerOptions]
         },
        ArenaComponent = pewpew_arena_component_factory:create(Options),
        ArenaComponent
    end,
    fun(ArenaComponent) ->
        pewpew_arena_component:stop(ArenaComponent)
    end,
    fun(ArenaComponent) ->
        ScanningPlayer = pewpew_arena_component:get_player(ArenaComponent, player_id_1),
        Scan   = pewpew_radar:scan(ArenaComponent, ScanningPlayer, 40),

        #{players := PlayersScan, walls := WallScan} = Scan,

        [
         ?_assertEqual([], PlayersScan),
         ?_assertEqual(Expectations, WallScan)
        ]
    end
  }.
