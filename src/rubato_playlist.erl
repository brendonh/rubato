%%%-------------------------------------------------------------------
%%% File    : rubato_playlist.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : 
%%%
%%% Created : 17 Aug 2009 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(rubato_playlist).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-include("rubato.hrl").

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
  playlist=[],
  current=none
}).


%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    ?DBG({?MODULE, running}),
    {ok, #state{}}.



handle_call(next, _From, #state{playlist=[Next|Rest]}=State) ->
    T = gen_server:call(rubato_lib, {trackInfo, Next}),
    JSON = {struct, [{file, T#track.file}, {artist, T#track.artist},
                     {album, T#track.album}, {title, T#track.title},
                     {track, T#track.track}]},
    Payload = iolist_to_binary(mochijson2:encode(JSON)),
    gen_server:cast(evosite_rubato_amqp, {send, <<"playing">>, Payload}),
    {reply, Next, State#state{playlist=Rest, current=T}};

handle_call(next, _From, #state{playlist=[]}=State) ->
    JSON = {struct, [{file, <<"">>}]},
    Payload = iolist_to_binary(mochijson2:encode(JSON)),
    gen_server:cast(evosite_rubato_amqp, {send, <<"playing">>, Payload}),
    {reply, <<"">>, State#state{current=none}};

handle_call(status, _From, State) ->
    {reply, {State#state.current, State#state.playlist}, State};

handle_call(Request, _From, State) ->
    ?DBG({unknown_call, Request}),
    {reply, ok, State}.


handle_cast({update, Playlist}, State) ->
    {noreply, State#state{playlist=Playlist}};

handle_cast(Msg, State) ->
    ?DBG({unknown_cast, Msg}),
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
