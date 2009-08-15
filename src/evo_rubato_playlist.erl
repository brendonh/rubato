%%%-------------------------------------------------------------------
%%% File    : evo_rubato_playlist.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : Evo playlist manipulation
%%%
%%% Created : 15 Aug 2009 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(evo_rubato_playlist).

-include("evo.hrl").

-export([nav/2, respond/5]).

-define(PAGE_TEMPLATE, "templates/playlist.html").


nav(Conf, _Args) -> 
    case evosession:user_info(Conf) of
        [] -> none;
        _ -> {"Playlist", [{"Playlist", "/playlist"}]}
    end.



respond(Req, Method, Path, Conf, Args) ->
    case evosession:user_info(Conf) of
        [] -> {response, Req:not_found()};
        _ -> respond2(Req, Method, Path, Conf, Args)
    end.


respond2(_Req, 'GET', [], _Conf, _Args) ->
    {ok, Content} = file:read_file(?PAGE_TEMPLATE),
    {wrap, site, [{content, Content}, {title, "Playlist"}]};


respond2(Req, 'GET', ["summary", "artists"], _Conf, _Args) ->
    Summary = gen_server:call(rubato_lib, artistSummary),
    JSON = mochijson2:encode({struct, Summary}),
    {response, Req:ok({"application/json", JSON})};


respond2(Req, _, _, _, _) ->
    {response, Req:not_found()}.

