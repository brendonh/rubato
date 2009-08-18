%%%-------------------------------------------------------------------
%%% File    : rubato_sup.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : 
%%%
%%% Created : 15 Aug 2009 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(rubato_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
init([]) ->
    Library = {library,{rubato_lib,start_link,[]},
               permanent,2000,worker,[rubato_lib]},

    Playlist = {playlist,{rubato_playlist,start_link,[]},
               permanent,2000,worker,[rubato_playlist]},

    LQS = {socket,{rubato_lqs,start_link,[]},
           permanent,2000,worker,[rubato_lqs]},

    {ok,{{one_for_one,1,1}, [Library, Playlist, LQS]}}.

%%====================================================================
%% Internal functions
%%====================================================================
