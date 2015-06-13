-module(pewpew_radar_tests).
-include_lib("eunit/include/eunit.hrl").

detects_no_player_when_there_are_no_players_test_() ->
  {setup,
    fun() ->
        application:set_env(pewpew, execution_mode, test),
        pewpew_config:load(),

        PlayersOptions = [[{id, player_id_1}]],
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
