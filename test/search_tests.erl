-module(search_tests).
-include_lib("eunit/include/eunit.hrl").

search_leaf_with_single_overlap_test_() ->
    Expected = point(3,3),
    Leaf = create_leaf([point(1,1), Expected, point(5,5)]),
    Search_Mbr = mbr:mbr([mbr:point_mbr([2,2]), mbr:point_mbr([4,4])]),
    ?_assertEqual([Expected],rtree:search(Search_Mbr, Leaf)).

search_leaf_with_multiple_overlap_test_() ->
    P1 = point(2,2),
    P2 = point(3,3),
    Expected = [P1,P2],
    Leaf = create_leaf([P1,P2,point(5,5)]), 
    Search_Mbr = mbr:mbr([mbr:point_mbr([1,1]), mbr:point_mbr([4,4])]),
    ?_assertEqual(Expected, rtree:search(Search_Mbr, Leaf)).

search_leaf_without_overlap_test_() ->
    Leaf = create_leaf([point(1,1), point(2,2), point(3,3)]),
    Search_Mbr = mbr:mbr([mbr:point_mbr([4,4]), mbr:point_mbr([5,5])]),
    ?_assertEqual([], rtree:search(Search_Mbr, Leaf)).
    
search_internal_with_single_overlap_test_() ->
    P1 = point(3,3),
    Leaf1 = create_leaf([point(1,1), P1]),
    Leaf2 = create_leaf([point(5,5), point(6,6)]),
    Node = create_node([Leaf1, Leaf2]),
    Search_Mbr = mbr:mbr([mbr:point_mbr([2,2]), mbr:point_mbr([4,4])]),
    ?_assertEqual([P1], rtree:search(Search_Mbr, Node)).

search_internal_with_multiple_overlap_test_() ->
    P1 = point(3,5),
    P2 = point(3,4),
    Leaf1 = create_leaf([P1, point(1,1)]),
    Leaf2 = create_leaf([P2, point(5,7)]),
    Node = create_node([Leaf1, Leaf2]),    
    Search_Mbr = mbr:mbr([mbr:point_mbr([2,3]), mbr:point_mbr([4,6])]),
    ?_assertEqual([P1,P2], rtree:search(Search_Mbr, Node)).


%% Helper function
point(X, Y) ->
    {mbr:point_mbr([X, Y]), {X, Y}}.

create_leaf(Content) ->
    node:create_leaf(Content).

create_node(Content) ->
    node:create_node(Content).
