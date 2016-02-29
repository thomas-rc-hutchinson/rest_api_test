-module(erl_expr_eval_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-compile([{parse_transform, lager_transform}]).
-record(state, {
}).

init(_, Req, _Opts) ->
        lager:info("[/eval] with body ~p", [expr_req(Req)]),
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
        {[{<<"expression">>,Exp}]} = expr_req(Req), 
        Resp = jiffy:encode(eval(Exp)),
	{ok, Req2} = cowboy_req:reply(200,  [{<<"content-type">>, <<"application/json">>}], Resp, Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.

expr_req(Req) ->
    {ok,Body,_} = cowboy_req:body(Req),
    jiffy:decode(Body).

eval(S) ->
    {ok,Scanned,_} = erl_scan:string(binary_to_list(S)),
    {ok,Parsed} = erl_parse:parse_exprs(Scanned),
    {value, Answer,_} = erl_eval:exprs(Parsed,[]),
    Answer.

