-module(pewpew_player_component).
-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([id/1, x/1, y/1, move/2, destroy/1, color/1, name/1, hit/1, life/1, rotate/2, rotation/1]).

-define(PROCESS_DOWN(Pid), {'DOWN', _MonitorRef, process, Pid, _}).
-define(MOVEMENT_SPEED, 1).

start_link(PewpewGameContextData, PlayerData) ->
  gen_server:start_link(?MODULE, [PewpewGameContextData, PlayerData], []).

id(PlayerComponent) ->
  gen_server:call(PlayerComponent, id).

x(PlayerComponent) ->
  gen_server:call(PlayerComponent, x).

y(PlayerComponent) ->
  gen_server:call(PlayerComponent, y).

hit(PlayerComponent) ->
  gen_server:cast(PlayerComponent, hit).

color(PlayerComponent) ->
  gen_server:call(PlayerComponent, color).

name(PlayerComponent) ->
  gen_server:call(PlayerComponent, name).

move(PlayerComponent, Data) ->
  gen_server:cast(PlayerComponent, {move, Data}).

destroy(PlayerComponent) ->
  gen_server:cast(PlayerComponent, destroy).

life(PlayerComponent) ->
  gen_server:call(PlayerComponent, life).

rotate(PlayerComponent, Data) ->
  gen_server:cast(PlayerComponent, {rotate, Data}).

rotation(PlayerComponent) ->
  gen_server:call(PlayerComponent, rotation).

init([PewpewGameContextData, PlayerData]) ->
  PlayerComponentData = pewpew_player_component_data:new(
    [{pewpew_game_context_data, PewpewGameContextData} | PlayerData]
  ),
  monitor_origin(PlayerComponentData),
  {ok, PlayerComponentData}.

handle_info(?PROCESS_DOWN(Pid), PlayerComponentData) ->
  react_to_process_down(PlayerComponentData, Pid).

handle_cast(destroy, PlayerComponentData) ->
  {stop, destroyed, PlayerComponentData};
handle_cast(hit, PlayerComponentData) ->
  {noreply, hit2(PlayerComponentData)};
handle_cast({move, Data}, PlayerComponentData) ->
  [{direction, Direction}] = Data,
  {noreply, move2(Direction, PlayerComponentData)};
handle_cast({rotate, Data}, PlayerComponentData) ->
  {noreply, pewpew_player_component_data:update(PlayerComponentData, [{rotation, Data}])}.

handle_call(id, _, PlayerComponentData) ->
  {reply, pewpew_player_component_data:id(PlayerComponentData), PlayerComponentData};
handle_call(x, _, PlayerComponentData) ->
  {reply, pewpew_player_component_data:x(PlayerComponentData), PlayerComponentData};
handle_call(y, _, PlayerComponentData) ->
  {reply, pewpew_player_component_data:y(PlayerComponentData), PlayerComponentData};
handle_call(color, _, PlayerComponentData) ->
  {reply, pewpew_player_component_data:color(PlayerComponentData), PlayerComponentData};
handle_call(life, _, PlayerComponentData) ->
  {reply, player_component_data_life(PlayerComponentData), PlayerComponentData};
handle_call(name, _, PlayerComponentData) ->
  {reply, pewpew_player_component_data:name(PlayerComponentData), PlayerComponentData};
handle_call(rotation, _, PlayerComponentData) ->
  {reply, pewpew_player_component_data:rotation(PlayerComponentData), PlayerComponentData}.

terminate(_Repos, _PlayerComponentData) ->
  die.

move2(<<"up">>, PlayerComponentData) ->
  DX = ?MOVEMENT_SPEED * math:cos(-pewpew_player_component_data:rotation(PlayerComponentData)),
  DY = ?MOVEMENT_SPEED * math:sin(-pewpew_player_component_data:rotation(PlayerComponentData)),
  move(<<"x">>, pewpew_player_component_data:x(PlayerComponentData) + DX, PlayerComponentData),
  move(<<"y">>, pewpew_player_component_data:y(PlayerComponentData) + DY, PlayerComponentData);
move2(<<"down">>, PlayerComponentData) ->
  DX = ?MOVEMENT_SPEED * math:cos(-pewpew_player_component_data:rotation(PlayerComponentData)),
  DY = ?MOVEMENT_SPEED * math:sin(-pewpew_player_component_data:rotation(PlayerComponentData)),
  move(<<"x">>, pewpew_player_component_data:x(PlayerComponentData) - DX, PlayerComponentData),
  move(<<"y">>, pewpew_player_component_data:y(PlayerComponentData) - DY, PlayerComponentData).

move(<<"x">>, Value, PlayerComponentData) ->
  pewpew_player_component_data:update(
    PlayerComponentData,
    [{x, Value}]
  );

move(<<"y">>, Value, PlayerComponentData) ->
  pewpew_player_component_data:update(
    PlayerComponentData,
    [{y, Value}]
  ).

hit2(PlayerComponentData) ->
  pewpew_player_component_data:update(
    PlayerComponentData,
    [{life, player_component_data_life(PlayerComponentData) - 10}]
  ).

player_component_data_life(PlayerComponentData) ->
  pewpew_player_component_data:life(PlayerComponentData).

monitor_origin(PlayerComponentData) ->
  erlang:monitor(process, pewpew_player_component_data:origin(PlayerComponentData)).

react_to_process_down(PlayerComponentData, Pid) ->
  react_to_process_down_if_origin(PlayerComponentData, pewpew_player_component_data:origin(PlayerComponentData), Pid).

react_to_process_down_if_origin(PlayerComponentData, Origin, Pid) when Origin =:= Pid ->
  pewpew_event_bus:trigger(pewpew_event_bus(PlayerComponentData), <<"player.disconnected">>, [[{id, pewpew_player_component_data:id(PlayerComponentData)}]]),
  {stop, origin_down, PlayerComponentData};
react_to_process_down_if_origin(PlayerComponentData, _, _) ->
  {noreply, PlayerComponentData}.

pewpew_event_bus(PlayerComponentData) ->
  pewpew_game_context_data:pewpew_event_bus(
    pewpew_player_component_data:pewpew_game_context_data(PlayerComponentData)
  ).
