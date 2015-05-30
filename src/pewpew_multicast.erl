-module(pewpew_multicast).
-behaviour(gen_server).

-export([
  start_link/0,
  stop/0,
  publish/2
]).

-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  terminate/2
]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?MODULE, stop).

publish(Data, Channels) ->
  gen_server:call(?MODULE, {publish, Data, Channels}).

init(_) ->
  {ok, undefined}.

handle_call({publish, Data, Channels}, _, State) ->
  publish_data(Data, Channels),
  {reply, ok, State}.

handle_cast(stop, State) ->
  {stop, normal, State}.

publish_data(Data, To) when is_pid(To) ->
  publish_data_to_channel(Data, To);
publish_data(Data, To) ->
  publish_data_to_channels(Data, To).

publish_data_to_channels(_Data, []) ->
  published;
publish_data_to_channels(Data, [Channel | Tail]) ->
  publish_data_to_channel(Data, Channel),
  publish_data_to_channels(Data, Tail).
publish_data_to_channel(Data, Channel) ->
  pewpew_channel:send(Channel, Data).

terminate(_Reason, _State) ->
  die.
