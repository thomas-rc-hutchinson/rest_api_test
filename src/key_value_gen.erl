%%%-------------------------------------------------------------------
%%% @author Thomas Hutchinson <thomashutchinson@thomasrchutchinson>
%%% @copyright (C) 2015, Thomas Hutchinson
%%% @doc
%%%
%%% @end
%%% Created : 18 Dec 2015 by Thomas Hutchinson <thomashutchinson@thomasrchutchinson>
%%%-------------------------------------------------------------------
-module(key_value_gen).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {keyvalue = #{}}).

-export([put/3,get/2,increment/3,increment/2,get_then_increment/2, get_then_increment/3]).

put(Key,Value,ServerPid) ->
    gen_server:call(ServerPid, {put,Key,Value}).

get(Key,ServerPid) ->
    gen_server:call(ServerPid, {get,Key}).

increment(Key,By,ServerPid) ->
    gen_server:cast(ServerPid, {increment,Key,By}).

increment(Key,ServerPid) ->
    gen_server:cast(ServerPid, {increment,Key}).

get_then_increment(Key,ServerPid) ->
    gen_server:call(ServerPid, {get_then_increment, Key}).

get_then_increment(Key,By,ServerPid) ->
    gen_server:call(ServerPid, {get_then_increment, Key,By}).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({get, Key}, _From, State=#state{keyvalue = KeyValue}) ->
    #{Key := Value} = KeyValue,
    Reply = Value,
    {reply, Reply, State};

handle_call({get_then_increment, Key, By}, _From, State=#state{keyvalue = KeyValue}) ->
    #{Key := Value} = KeyValue,
    Reply = Value,
    {reply, Reply, mutate_value(Key, increment_fun(By), State)};

handle_call({get_then_increment, Key}, _From, State=#state{keyvalue = KeyValue}) ->
    #{Key := Value} = KeyValue,
    Reply = Value,
    {reply, Reply, mutate_value(Key, increment_fun(1), State)};


handle_call({put, Key, Value}, _From, State=#state{keyvalue = KeyValue}) ->
    State2 = State#state{keyvalue=maps:merge(#{Key=>Value}, KeyValue)},
    {reply, ok, State2}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({increment,Key,By},State) ->
    {noreply, mutate_value(Key,increment_fun(By),State)};

handle_cast({increment,Key}, State) ->
    {noreply, mutate_value(Key,increment_fun(1),State)}.

increment_fun(By) ->
    fun(Value) ->
	    Value + By end.

mutate_value(Key,MutationFun,State=#state{keyvalue = KeyValue}) ->
    #{Key := Value} = KeyValue,
    State#state{keyvalue = maps:update(Key,MutationFun(Value),KeyValue)}.




%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
