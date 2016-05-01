-module(ping_handler).
-behaviour(cowboy_http_handler).
-compile([{parse_transform, lager_transform}]).
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).


init(_, Req, [KeyValuePid]) ->
        ReqId = request:create_req_id(KeyValuePid),
        lager:info("[/ping] [ReqId=~p Qs=~p]~n", [ReqId, qs(Req)]),
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
        timer:sleep(100),
	{ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], 
            <<"OK">>, Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.



qs(Req) ->
    {Qs,_} = cowboy_req:qs(Req),
    Qs.
