%%%-------------------------------------------------------------------
%%% File    : rubato_app.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : 
%%%
%%% Created : 15 Aug 2009 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(rubato_app).

-behaviour(application).

%% API
-export([go/0, setup/0]).

%% Application callbacks
-export([start/2, stop/1]).

-include("rubato.hrl").

go() ->
    mnesia:start(),
    application:start(rubato),
    application:start(evo).

setup() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:wait_for_tables([track], 1000),
    ?DBG(mnesia:delete_table(track)),
    ?DBG(mnesia:create_table(track, [{attributes, record_info(fields, track)},
                                     {disc_copies, [node()]}])),
    mnesia:info().


%%====================================================================
%% Application callbacks
%%====================================================================
start(_Type, _StartArgs) ->
    case rubato_sup:start_link() of
        {ok, Pid} -> 
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
