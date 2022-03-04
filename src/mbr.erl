-module(mbr).
-include_lib("rtree.hrl").
-export([
	 point_mbr/1,
	 mbr/1,
	 area/1,
	 intersection/2
	]).

-export_type([
	      mbr/0
	     ]).

-type dimension()  :: {integer(), integer()}. % min & max value for a single dimension
-type mbr()  :: list(dimension()). % mbr is described by its extent in each dimension

% Create MBR for N-dimensional point
-spec point_mbr(list(integer())) -> mbr().
point_mbr(Coords) ->
    [{X, X} || X <- Coords].

% Get mbr of a list of mbrs
-spec mbr(list(mbr())) -> mbr().
mbr([H|T]) ->
    mbr_rec(T, H).

-spec mbr_rec(list(mbr()), mbr()) -> mbr().
mbr_rec([], Final_mbr) -> Final_mbr;
mbr_rec([H|T], Current_mbr) ->
    mbr_rec(T, bounds(H, Current_mbr, [])).

% Get mbr of two other mbrs
-spec bounds(mbr(), mbr(), mbr()) -> mbr().
bounds([], [], Final_mbr) -> lists:reverse(Final_mbr);
bounds([H1|T1], [H2|T2], Mbr_acc) ->
    bounds(T1, T2, [dimensional_bounds(H1, H2) | Mbr_acc]).

-spec dimensional_bounds(dimension(), dimension()) -> dimension().
dimensional_bounds({Min1, Max1}, {Min2, Max2}) ->
    {min(Min1, Min2), max(Max1, Max2)}.


-spec area(mbr()) -> integer().
area([H|T]) ->
    area_rec(T, dim_length(H)).

-spec area_rec(list(dimension()), integer()) -> integer().
area_rec([], Acc) -> Acc;
area_rec([H|T], Acc) -> 
    area_rec(T, dim_length(H)*Acc).

-spec dim_length(dimension()) -> integer().
dim_length({Start, End}) ->
    End - Start.

-spec intersection(mbr(), mbr()) -> mbr().
intersection(M1, M2) ->
    intersection_rec(M1, M2, []).

intersection_rec([],[],Acc) -> Acc;
intersection_rec([{Min1,Max1}|T1],[{Min2,Max2}|T2], Acc) ->
    intersection_rec(T1,T2, [{max(Min1, Min2), min(Max1, Max2)}|Acc]).
