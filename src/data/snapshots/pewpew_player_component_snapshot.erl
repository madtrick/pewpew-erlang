-module(pewpew_player_component_snapshot).

-export([
  new/1
]).

new(PlayerComponentData) ->
  Id              = pewpew_player_component_data:id(PlayerComponentData),
  X               = pewpew_player_component_data:x(PlayerComponentData),
  Y               = pewpew_player_component_data:y(PlayerComponentData),
  Rotation        = pewpew_player_component_data:rotation(PlayerComponentData),
  Life            = pewpew_player_component_data:life(PlayerComponentData),
  RadarConfigData = pewpew_player_component_data:radar_config_data(PlayerComponentData),
  RadarMode       = pewpew_radar_config_data:mode(RadarConfigData),
  RadarRadius     = pewpew_radar_config_data:radius(RadarConfigData),

  #{
     id => Id,
     coordinates => #{ x => X, y => Y },
     rotation => Rotation,
     life => Life,
     radar => #{type => RadarMode, radius => RadarRadius}
  }.
