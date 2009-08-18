%%%-------------------------------------------------------------------
%%% File    : rubato_socket.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : Write next song filename to a socket
%%%
%%% Created : 18 Aug 2009 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(rubato_lqs).

-behaviour(gen_server).

-include("rubato.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {listener, acceptor}).

-define(PORT, 1358).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    process_flag(trap_exit, true),

    %Command = lists:flatten(io_lib:format("~s ~s ~s ~s ~B", Args)),

    %Dir = ?GV(dir, ArgConf),

    %Port = open_port({spawn, Command}, 
    %                 [{cd, Dir}, use_stdio, 
    %                  binary, stream, {line, 1024}, 
    %                  eof, exit_status]),

    case gen_tcp:listen(?PORT, [{reuseaddr, true}, 
                                {active, false}, 
                                {packet, line}]) of
        {ok, Listen_socket} ->
            %%Create first accepting process
            {ok, Ref} = prim_inet:async_accept(Listen_socket, -1),
            ?DBG({?MODULE, listening, ?PORT}),
            {ok, #state{listener = Listen_socket,
                        acceptor = Ref}};
        {error, Reason} ->
            {stop, Reason}
    end.


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({inet_async, ListSock, Ref, {ok, CliSocket}},
            #state{listener=ListSock, acceptor=Ref} = State) ->
    try
        case set_sockopt(ListSock, CliSocket) of
            ok              -> ok;
            {error, Reason} -> exit({set_sockopt, Reason})
        end,
        
        %{ok, Time} = gen_tcp:recv(CliSocket, 0),
        %?DBG({time, Time}),

        Next = gen_server:call(rubato_playlist, next),

        ?DBG({giving, Next}),

        ok = gen_tcp:send(CliSocket, 
                          concat_binary(
                            [<<"file://">>, Next, <<"\n">>])),
        gen_tcp:close(CliSocket),

        case prim_inet:async_accept(ListSock, -1) of
            {ok,    NewRef} -> ok;
            {error, NewRef} -> exit({async_accept, inet:format_error(NewRef)})
        end,
        
        {noreply, State#state{acceptor=NewRef}}
    catch exit:Why ->
            error_logger:error_msg("Error in async accept: ~p.\n", [Why]),
            {stop, Why, State}
    end;

handle_info(Info, State) ->
    ?DBG({info, Info}),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% Taken from prim_inet.  We are merely copying some socket options from the
%% listening socket to the new client socket.
set_sockopt(ListSock, CliSocket) ->
    true = inet_db:register_socket(CliSocket, inet_tcp),
    case prim_inet:getopts(ListSock, [active, nodelay, keepalive, delay_send, priority, tos]) of
    {ok, Opts} ->
        case prim_inet:setopts(CliSocket, Opts) of
        ok    -> ok;
        Error -> gen_tcp:close(CliSocket), Error
        end;
    Error ->
        gen_tcp:close(CliSocket), Error
    end.
