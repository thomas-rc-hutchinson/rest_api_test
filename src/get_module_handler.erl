-module(get_module_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
        {Module, _} = cowboy_req:binding(module, Req),
        ModuleAtom = list_to_atom(binary_to_list(Module)),
        Exports = con(ModuleAtom:module_info(exports)),

        Exp = ["Exports:</br>"] ++ [<<M/binary,":",A/binary,"</br>">> || {M,A} <- Exports],
        
	{ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/html">>}],
        Exp, Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.

con(Exports) ->
    [{bin(M),bin(A)} || {M,A} <- Exports].

bin(Atom) when is_atom(Atom) ->
    list_to_binary(atom_to_list(Atom));
bin(Int) when is_integer(Int) ->
    integer_to_binary(Int).
