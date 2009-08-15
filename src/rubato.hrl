%%%-------------------------------------------------------------------
%%% File    : rubato.hrl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : Utilities
%%%
%%% Created : 15 Aug 2009 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------

-define(GV(E, P), proplists:get_value(E, P)).
-define(GVD(E, P, D), proplists:get_value(E, P, D)).
-define(DBG(Term), io:format("~p: ~p~n", [self(), Term])).


-record(track, {
  file,
  artist,
  album,
  title,
  track,
  length
}).
