-module(mbr_tests).
-include_lib("eunit/include/eunit.hrl").

point_mbr_test_() ->
    [
     ?_assertEqual([{1,1},{2,2}], mbr:point_mbr([1, 2])),
     ?_assertEqual([{4,4},{7,7},{9,9},{2,2}], mbr:point_mbr([4, 7, 9, 2]))
    ].

mbr_test_() ->
    [
     ?_assertEqual(
	[{1,1}, {2,2}],
	mbr([mbr:point_mbr([1,2])])),
     ?_assertEqual(
	[{1,2},{1,2}],
	mbr([mbr:point_mbr([1,1]), mbr:point_mbr([2,2])])),
     ?_assertEqual(
	[{1,3},{2,4},{3,5}],
	mbr([
		   mbr:point_mbr([1,2,4]),
		   mbr:point_mbr([3,2,3]),
		   mbr:point_mbr([2,4,5])]))
    ].

area_test_() ->
    [
     ?_assertEqual(4, mbr:area(rect(2, 4))),
     ?_assertEqual(36, mbr:area(mbr([rect(2,4), rect(6,8)]))),
     ?_assertEqual(0, mbr:area(mbr(points([[1,1,1,1]]))))
    ].

intersection_test_() ->
    [
     ?_assertEqual(rect(2,3), mbr:intersection(rect(1,3), rect(2,4))),
     ?_assertEqual([{3,3},{6,4}],mbr:intersection([{1,4},{3,5}], [{6,9},{1,3}]))
    ].

%% helper functions
mbr(X) -> mbr:mbr(X).

rect(X, Y) ->
    mbr:mbr([mbr:point_mbr([X,X]), mbr:point_mbr([Y,Y])]).

points(List) ->
    [mbr:point_mbr(SubList) || SubList <- List].
