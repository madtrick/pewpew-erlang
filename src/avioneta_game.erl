-module(avioneta_game).
-behaviour(gen_server).

-export([start_link/1, arena_component/1]).
-export([init/1, handle_call/3, handle_info/2]).

start_link(GameName) ->
  gen_server:start_link({local, GameName}, ?MODULE, [], []).

arena_component(AvionetaGame) ->
  gen_server:call(AvionetaGame, arena_component).

init(_) ->
  {ok, EventBus}          = avioneta_event_bus:start_link(),
  AvionetaGameContextData = avioneta_game_context_data:new([{avioneta_event_bus, EventBus}]),
  {ok, ArenaComponent}    = avioneta_arena_component:start_link([{avioneta_game_context_data, AvionetaGameContextData}, {width, arena_width()}, {height, arena_height()}]),
  {ok, avioneta_game_state_data:new([{avioneta_arena_component, ArenaComponent}, {avioneta_game_context_data, AvionetaGameContextData}]), 0}.

handle_info(timeout, AvionetaGameStateData) ->
  avioneta_event_bus:on(avioneta_event_bus(AvionetaGameStateData), <<"player.disconnected">>, fun(Data) ->
        spawn(fun() ->
              JSON = jiffy:encode({[{type, <<"DisconnectPlayerOrder">>}, {data, {[{id, proplists:get_value(id, Data)}]}}]}),
              avioneta_multicast:publish(JSON, avioneta_registry:entries())
          end)
  end),
  {noreply, AvionetaGameStateData}.

handle_call(arena_component, _, AvionetaGameStateData) ->
  {reply, avioneta_game_state_data:avioneta_arena_component(AvionetaGameStateData), AvionetaGameStateData}.

avioneta_event_bus(AvionetaGameStateData) ->
  AvionetaGameContextData = avioneta_game_state_data:avioneta_game_context_data(AvionetaGameStateData),
  avioneta_game_context_data:avioneta_event_bus(AvionetaGameContextData).

arena_width() ->
  avioneta_config:get('arena.width').

arena_height() ->
  avioneta_config:get('arena.height').
