%%%-------------------------------------------------------------------
%%% File    : rubato_lib.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : Rubato library management
%%%
%%% Created : 15 Aug 2009 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(rubato_lib).

-behaviour(gen_server).

%% API
-export([start_link/0, test/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("rubato.hrl").
-include("qlc.hrl"). 

-record(state, {}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

test() ->
    gen_server:cast(?MODULE, rescan),
    ?DBG(gen_server:call(?MODULE, artistSummary)).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    case mnesia:wait_for_tables([track], 5000) of
        ok -> 
            ?DBG({?MODULE, running}),
            {ok, #state{}};
        Other ->
            ?DBG({oh_noes, Other}),
            {stop, no_tracks_table}
    end.



handle_call(artistSummary, _From, State) ->
    {reply, artist_summary(), State};

handle_call({albumsByArtist, Artist}, _From, State) ->
    {reply, artist_albums(Artist), State};

handle_call({songsByAlbum, Artist, Album}, _From, State) ->
    {reply, album_songs(Artist, Album), State};

handle_call(Request, _From, State) ->
    ?DBG({unknown_call, Request}),
    {reply, ok, State}.



handle_cast(rescan, State) ->
    rescan(),
    {noreply, State};

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

rescan() ->
    {ok, Patterns} = application:get_env(lib_patterns),
    mnesia:clear_table(track),
    ?DBG(mnesia:transaction(fun() -> rescan(Patterns) end)).

rescan([Pattern|Rest]) ->
    ?DBG({scanning, Pattern}),
    scan_files(filelib:wildcard(Pattern), Rest);
rescan([]) ->
    ?DBG(rescan_finished).

scan_files([File|Rest], MorePatterns) ->
    case audinfo:get_info(File) of
        {Artist, Album, Title, Track, Length} ->
            Record = #track{file=list_to_binary(File), 
                            artist=Artist, album=Album, 
                            title=Title, track=Track,
                            length=Length},
            mnesia:write(Record);
        undefined ->
            %?DBG({skipping, File})
            ok
    end,
    scan_files(Rest, MorePatterns);
scan_files([], MorePatterns) ->
    rescan(MorePatterns).



artist_summary() ->
    {atomic, D} = mnesia:transaction(fun build_artist_summary/0),
    dict:to_list(D).

build_artist_summary() ->
    mnesia:foldl(fun(Track, Dict) ->
                         dict:update(Track#track.artist,
                                     fun(C) -> C+1 end,
                                     1, Dict)
                 end, dict:new(), track).


artist_albums(Artist) ->
    {atomic, D} = mnesia:transaction(fun() -> build_artist_albums(Artist) end),
    dict:to_list(D).

build_artist_albums(Artist) ->
    Q = qlc:q([T || T <- mnesia:table(track),
                    T#track.artist == Artist]),
    lists:foldl(fun(Track, Dict) ->
                        dict:update(Track#track.album,
                                    fun(C) -> C+1 end,
                                    1, Dict)
                end, dict:new(), qlc:e(Q)).
 

album_songs(Artist, Album) ->                        
    {atomic, Songs} = mnesia:transaction(fun() -> build_album_songs(Artist, Album) end),
    Songs.
    
build_album_songs(Artist, Album) ->
    Q = qlc:q([T || T <- mnesia:table(track),
                    T#track.artist == Artist,
                    T#track.album == Album]),
    qlc:e(Q).

%%--------------------------------------------------------------------
%%% Debug functions
%%--------------------------------------------------------------------

adjust_name(undefined) -> <<"???">>;
adjust_name(Other) -> Other.
    
adjust_track(undefined) -> "";
adjust_track(Other) -> integer_to_list(Other).
