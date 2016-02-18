-module(node_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, [KeyValuePid]) ->
	ReqId = request:create_req_id(KeyValuePid),
        io:format("[/node] [ReqId=~p]~n", [ReqId]),
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
        Node = list_to_binary(atom_to_list(node())),
    {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}],
            Node, Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.
