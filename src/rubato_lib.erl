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
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("rubato.hrl").

-record(state, {}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.



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
            ?DBG({skipping, File})
    end,
    scan_files(Rest, MorePatterns);
scan_files([], MorePatterns) ->
    rescan(MorePatterns).




%%--------------------------------------------------------------------
%%% Debug functions
%%--------------------------------------------------------------------

adjust_name(undefined) -> <<"???">>;
adjust_name(Other) -> Other.
    
adjust_track(undefined) -> "";
adjust_track(Other) -> integer_to_list(Other).
