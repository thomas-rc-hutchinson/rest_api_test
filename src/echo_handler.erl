-module(echo_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).


init(_, Req, [KeyValuePid]) ->
        ReqId = request:create_req_id(KeyValuePid),
        io:format("[/echo] [ReqId=~p Qs=~p]~n", [ReqId, qs(Req)]),
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
        timer:sleep(100),
	{ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], 
            qs_value(<<"value">>, Req), Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.

%% Get query string value
qs_value(Key,Req) ->
    {Value,_} = cowboy_req:qs_val(Key,Req),
    Value.

qs(Req) ->
    {Qs,_} = cowboy_req:qs(Req),
    Qs.
