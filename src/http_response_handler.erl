-module(http_response_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, [KeyValuePid]) ->
        ReqId = request:create_req_id(KeyValuePid),
        io:format("[/http_response_handler] [ReqId=~p Qs=~p]~n", [ReqId, qs(Req)]),
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
	{ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], 
            qs_value(<<"response_status">>, Req), Req),
	{ok, Req2, State}.

%% Get query string value
qs_value(Key,Req) ->
    {Value,_} = cowboy_req:qs_val(Key,Req),
    Value.

qs(Req) ->
    {Qs,_} = cowboy_req:qs(Req),
    Qs.

terminate(_Reason, _Req, _State) ->
	ok.
