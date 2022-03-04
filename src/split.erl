-module(split).
-include_lib("rtree.hrl").
-export([
	 exhaustive/2
	]).

-spec exhaustive(list(rtree:rtree()), atom()) -> list(rtree:rtree()).
exhaustive(Elems, Type) ->
    Splits = split_elements(Elems),
    {E1,E2} = smallest_combined_area(Splits),
    [{Type, E1},{Type,E2}].

%-spec split_elements(list(T)) -> list({T, T}).
split_elements(List) ->
    split_elements_rec(List, 1).
split_elements_rec(List, Count) when Count > round(length(List)/2) ->
    [];
split_elements_rec(List, Count) ->
    combination(Count, List) ++ split_elements_rec(List, Count+1).

% Create all possible sublists of a certain length
combination(0, L) -> [{[], L}];
combination(_, []) -> [];
combination(N, [X|XS]) -> 
    TS = [{[X|YS],ZS} || {YS, ZS} <- combination(N-1, XS)],
    DS = [{YS, [X|ZS]} || {YS, ZS} <- combination(N, XS)],
    TS ++ DS.

%-spec smallest_combined_area(list(pair(list(rtree())))) -> pair(rtree()).
smallest_combined_area(List) ->
    [H|T] = combined_areas(List, []),
    {_Area, Elems} = lists:foldl(fun(X, Acc) -> smallest(X, Acc) end, H,T),
    Elems.

%-spec combined_areas(list(pair(list(rtree()))), list()) -> list({integer(), pair(list(rtree()))}).
combined_areas([], Acc) -> Acc;
combined_areas([H|T], Acc) ->
    combined_areas(T, [{combined_area(H), H}|Acc]).

combined_area({X,Y}) ->
    node:area(X) + node:area(Y).

smallest({Area1, _Elems1} = X, {Area2, _Elems2} = Y) ->
    case Area1 < Area2 of
	true -> X;
	false -> Y
    end.

