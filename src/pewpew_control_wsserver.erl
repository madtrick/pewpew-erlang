-module(pewpew_control_wsserver).

-export([config/0]).

-define(PORT_ENV_VARIABLE, "CONTROL_PORT").

config() ->
  [
   {port, port()},
   {number_of_workers, number_of_workers()},
   {worker_options, [
      {protocol_modules_options, protocol_modules_options()}
    ]
   }].

port() ->
  PortENVVariable = fserlangutils_string:to_integer(os:getenv(?PORT_ENV_VARIABLE)),
  pewpew_config:get([control_wsserver, port], PortENVVariable).

number_of_workers() ->
  pewpew_config:get([control_wsserver, workers]).

protocol_modules_options() ->
  [
    {wsserver_websocket_protocol,[
        {handler_module, pewpew_wsserver_control_handler}
      ]
    }
  ].

