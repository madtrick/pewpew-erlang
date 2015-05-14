-module(pewpew_game).
-behaviour(gen_server).

-export([start_link/1, arena_component/1]).
-export([init/1, handle_call/3, handle_info/2]).

start_link(GameName) ->
  gen_server:start_link({local, GameName}, ?MODULE, [], []).

arena_component(PewpewGame) ->
  gen_server:call(PewpewGame, arena_component).

init(_) ->
  {ok, EventBus}        = pewpew_event_bus:start_link(),
  PewpewGameContextData = pewpew_game_context_data:new([{pewpew_event_bus, EventBus}]),
  {ok, ArenaComponent}  = pewpew_arena_component:start_link([{pewpew_game_context_data, PewpewGameContextData}, {width, arena_width()}, {height, arena_height()}]),
  {ok, pewpew_game_state_data:new([{pewpew_arena_component, ArenaComponent}, {pewpew_game_context_data, PewpewGameContextData}]), 0}.

handle_info(timeout, PewpewGameStateData) ->
  pewpew_event_bus:on(pewpew_event_bus(PewpewGameStateData), <<"player.disconnected">>, fun(Data) ->
        spawn(fun() ->
              JSON = jiffy:encode({[{type, <<"DisconnectPlayerOrder">>}, {data, {[{id, proplists:get_value(id, Data)}]}}]}),
              pewpew_multicast:publish(JSON, pewpew_registry:entries())
          end)
  end),
  {noreply, PewpewGameStateData}.

handle_call(arena_component, _, PewpewGameStateData) ->
  {reply, pewpew_game_state_data:pewpew_arena_component(PewpewGameStateData), PewpewGameStateData}.

pewpew_event_bus(PewpewGameStateData) ->
  PewpewGameContextData = pewpew_game_state_data:pewpew_game_context_data(PewpewGameStateData),
  pewpew_game_context_data:pewpew_event_bus(PewpewGameContextData).

arena_width() ->
  pewpew_config:get('arena.width').

arena_height() ->
  pewpew_config:get('arena.height').
