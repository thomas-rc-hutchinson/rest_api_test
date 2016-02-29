-module(module_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
        Modules = [list_to_binary(atom_to_list(Mod)) || {Mod,_} <- code:all_loaded()],
        {ok, Req2} = cowboy_req:reply(200,  [{<<"content-type">>, <<"text/html">>}],
        to_html(Modules), Req),
	{ok, Req2, State}.


to_html(Modules) ->
    [<<"<a href=\"http://127.0.0.1:8085/module/",
           M/binary, "\">", M/binary, "</a></br>">> || M <- Modules].

terminate(_Reason, _Req, _State) ->
	ok.
