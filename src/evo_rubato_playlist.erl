%%%-------------------------------------------------------------------
%%% File    : evo_rubato_playlist.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : Evo playlist manipulation
%%%
%%% Created : 15 Aug 2009 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(evo_rubato_playlist).

%-include("evo.hrl").
-include("rubato.hrl").

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

respond2(Req, 'GET', ["summary", "albums"], _Conf, _Args) ->
    QS = mochiweb_request:parse_qs(Req),
    Artist = list_to_binary(?GV("artist", QS)),
    AlbumSummary = gen_server:call(rubato_lib, {albumsByArtist, Artist}),
    JSON = mochijson2:encode({struct, AlbumSummary}),
    {response, Req:ok({"application/json", JSON})};

respond2(Req, 'GET', ["summary", "songs"], _Conf, _Args) ->
    QS = mochiweb_request:parse_qs(Req),
    Artist = list_to_binary(?GV("artist", QS)),
    Album = list_to_binary(?GV("album", QS)),
    Songs = gen_server:call(rubato_lib, {songsByAlbum, Artist, Album}),

    TrackMaybe = fun(undefined) -> <<"">>;
                    (X) -> X end,
    
    SongObjects = [{struct, [{artist, T#track.artist},
                             {album, T#track.album},
                             {title, T#track.title}, 
                             {track, TrackMaybe(T#track.track)},
                             {file, T#track.file}]}
                   || T <- Songs],

    JSON = mochijson2:encode({struct, [{songs, SongObjects}]}),
    {response, Req:ok({"application/json", JSON})};

respond2(Req, 'POST', ["edit"], _Conf, _Args) ->
    Form = [{list_to_atom(K), list_to_binary(V)} || {K, V} <- Req:parse_post()],
    Track = #track{file=?GV(file, Form),
                   artist=?GV(artist, Form),
                   album=?GV(album, Form),
                   title=?GV(title, Form),
                   track=?GV(track, Form)},
    gen_server:cast(rubato_lib, {editInfo, Track}),
    {response, Req:ok({"application/json", "{}"})};

respond2(Req, _, _, _, _) ->
    {response, Req:not_found()}.

