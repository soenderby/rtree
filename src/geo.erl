-module(geo).

-export([
	 point/2
	]).

point(X,Y) ->
    {mbr:point_mbr([X,Y]), {X,Y}}.
