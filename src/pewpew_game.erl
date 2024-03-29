-module(pewpew_game).
-behaviour(gen_server).

-export([
  start_link/1,
  start_game/1,
  is_started/1,
  arena_component/1,
  snapshot/1,
  update/1
]).

-export([
  init/1,
  handle_cast/2,
  handle_call/3
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(GameName) ->
  gen_server:start_link({local, GameName}, ?MODULE, [], []).

start_game(PewpewGame) ->
  gen_server:cast(PewpewGame, start).

is_started(PewpewGame) ->
  gen_server:call(PewpewGame, is_started).

arena_component(PewpewGame) ->
  gen_server:call(PewpewGame, arena_component).

snapshot(PewPewGame) ->
  gen_server:call(PewPewGame, snapshot).

update(PewPewGame) ->
  gen_server:call(PewPewGame, update).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_) ->
  {ok, ArenaComponent}  = pewpew_arena_component:start_link([{width, arena_width()}, {height, arena_height()}]),
  PewPewGameStateData   = pewpew_game_state_data:new([{pewpew_arena_component, ArenaComponent}]),

  {ok, PewPewGameStateData}.

handle_cast(start, PewpewGameStateData) ->
  UpdatedPewPewGameStateData = pewpew_game_state_data:update(PewpewGameStateData, [{pewpew_game_status, started}]),
  {noreply, UpdatedPewPewGameStateData}.

handle_call(update, _, PewPewGameStateData) ->
  ArenaComponent = pewpew_game_state_data:pewpew_arena_component(PewPewGameStateData),
  Messages       = pewpew_arena_component:update(ArenaComponent),
  {reply, Messages, PewPewGameStateData};
handle_call(snapshot, _, PewPewGameStateData) ->
  Snapshot = pewpew_game_snapshot:new(PewPewGameStateData),
  {reply, Snapshot, PewPewGameStateData};
handle_call(is_started, _, PewpewGameStateData) ->
  GameStatus = pewpew_game_state_data:pewpew_game_status(PewpewGameStateData),
  IsStarted = GameStatus == started,
  {reply, IsStarted, PewpewGameStateData};
handle_call(arena_component, _, PewpewGameStateData) ->
  {reply, pewpew_game_state_data:pewpew_arena_component(PewpewGameStateData), PewpewGameStateData}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
arena_width() ->
  pewpew_config:get([arena, width]).

arena_height() ->
  pewpew_config:get([arena, height]).
