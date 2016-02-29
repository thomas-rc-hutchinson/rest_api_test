-module(rest_api_test_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
        {ok,KeyValuePid} = key_value_gen:start_link(),
        key_value_gen:put("sleep_millis",100,KeyValuePid),
        key_value_gen:put("req_id",1,KeyValuePid),
        IncreaseBy = 10,

        %%hack to ensure initial values are set
        timer:sleep(100),


	Dispatch = cowboy_router:compile([
        {'_', [{"/increase_sleep", increase_sleep_handler, [KeyValuePid,IncreaseBy]},
               {"/http_response", http_response_handler, [KeyValuePid]},
               {"/node", node_handler, [KeyValuePid]},
               {"/echo", echo_handler, [KeyValuePid]},
               {"/rpc", rpc_handler, []},
               {"/eval", erl_expr_eval_handler, []},
               {"/module", module_handler, []},
               {"/module/:module", get_module_handler, []}]}]),
					 
        cowboy:start_http(my_http_server, 100, 
        [{port, 8085}],[{env, [{dispatch, Dispatch}]}]),
        rest_api_test_sup:start_link().

stop(_State) ->
	ok.
