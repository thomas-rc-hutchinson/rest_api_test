-module(rpc_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-compile([{parse_transform, lager_transform}]).
-record(state, {
}).

init(_, Req, _Opts) ->
        lager:info("[/rpc] with body ~p", [rpc_req(Req)]),
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
        {[{<<"module">>,Mod},{<<"function">>,Fun},{<<"args">>,Args}]} = rpc_req(Req),
        Result = erlang:apply(atom(Mod), atom(Fun), Args),
        lager:info("[/rpc] result is ~p", [Result]),
	{ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], 
                jiffy:encode({[{<<"result">>, prepare_result(Result)}]}), Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.

atom(Bin) ->
    list_to_atom(binary_to_list(Bin)).

%%extreamly naive, will break if tuple is present
%%in term
prepare_result(Tuple) when is_tuple(Tuple) ->
    lists:flatten(io_lib:format("~p", [Tuple]));
prepare_result(Result) ->
    Result.
    

rpc_req(Req) ->
    {ok,Body,_} = cowboy_req:body(Req),
    jiffy:decode(Body).
