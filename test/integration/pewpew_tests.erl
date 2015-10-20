-module(pewpew_tests).
-include_lib("eunit/include/eunit.hrl").

all_test_() ->
  pewpew_test_runner:run([
        pewpew_game_start_tests:tests(),
        pewpew_player_registration_tests:tests(),
        pewpew_player_movement_tests:tests(),
        pewpew_game_snapshot_notification_tests:tests(),
        pewpew_radar_scan_notification_tests:tests(),
        pewpew_shooting_tests:tests()
      ]).
