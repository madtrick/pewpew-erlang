-module(avioneta_registry).
-behaviour(gen_server).

-export([start_link/0, register/1, entries/0]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2]).

-define(GEN_SERVER_NAME, avioneta_registry).

-record(registry, { entries = [] }).

start_link() ->
  gen_server:start_link({local, ?GEN_SERVER_NAME}, ?MODULE, [], []).

register(Pid) ->
  gen_server:cast(?GEN_SERVER_NAME, {register, Pid}).

entries() ->
  gen_server:call(?GEN_SERVER_NAME, entries).

init(_) ->
  {ok, #registry{}}.

handle_cast({register, Pid}, State) ->
  lager:debug("Add process to registry (~w)", [Pid]),
  %
  % NOTE!!!
  %
  % monitoring the process is bad idea
  %
  % But this is the quick way of removind dead workers from the
  % registry as currently they can't notify their exit to the handler
  % module
  %
  monitor(process, Pid),
  {noreply, State#registry{ entries = [ Pid | State#registry.entries ] }}.

handle_call(entries, _, State) ->
  {reply, State#registry.entries, State}.

handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, State) ->
  lager:debug("Remove process from registry (~w)", [Pid]),
  {noreply, State#registry{ entries = lists:delete(Pid, State#registry.entries) }}.
