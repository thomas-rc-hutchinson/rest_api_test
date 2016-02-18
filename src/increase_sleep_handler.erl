-module(increase_sleep_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {keyvaluepid, increaseby, req_id, sleep}).
-define(SLEEP_KEY, "sleep_millis").

init(_, Req, [KeyValuePid,IncreaseBy]) ->        
        ReqId = request:create_req_id(KeyValuePid),
        SleepFor = key_value_gen:get_then_increment(?SLEEP_KEY, IncreaseBy, KeyValuePid),
        io:format("[/increase_sleep] [ReqId=~p KeyValuePid=~p, IncreaseBy=~p SleepFor=~p]~n", 
            [ReqId, KeyValuePid,IncreaseBy,SleepFor]),
	{ok, Req, #state{keyvaluepid=KeyValuePid, increaseby=IncreaseBy, 
        req_id=ReqId,sleep=SleepFor}}.

handle(Req, State = #state{req_id=ReqId, sleep=SleepFor}) ->
        io:format("Sleeping... [ReqId=~p SleepFor=~p]~n",[ReqId,SleepFor]),
        timer:sleep(SleepFor),
        io:format("Awoken... [ReqId=~p SleepFor=~p]~n",[ReqId,SleepFor]),
	{ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], 
            integer_to_binary(SleepFor),Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.


