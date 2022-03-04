-module(delete_tests).
-include_lib("eunit/include/eunit.hrl").

delete_leaf_test_() ->
    ExpectedLeaf = create_leaf([point(1,1), point(2,2)]),
    Leaf = create_leaf([point(1,1), point(2,2), point(8,8)]),
    ?_assertEqual(ExpectedLeaf, rtree:delete(point(8,8), Leaf)).

delete_internal_node_test_() ->
    Leaf1 = create_leaf([point(1,1), point(2,2), point(3,3)]),
    Leaf2 = create_leaf([point(8,8), point(9,9)]),
    Expected = create_node([
			    Leaf2,
			    create_leaf([point(2,2), point(3,3)])
			    ]),
    ?_assertEqual(Expected, rtree:delete(point(1,1), create_node([Leaf1, Leaf2]))).

delete_remove_underflowing_node_test_() ->
    ExpectedLeaf = create_leaf([point(2,2), point(3,3), point(4,4)]),

    Leaf1 = create_leaf([point(1,1), point(2,2)]),
    Leaf2 = create_leaf([point(3,3), point(4,4)]),
    Leaf3 = create_leaf([point(8,8), point(9,9)]),
    Expected = create_node([ExpectedLeaf, Leaf3]),
    ?_assertEqual(Expected, rtree:delete(point(1,1), create_node([Leaf1, Leaf2, Leaf3]))).

delete_adjust_root_with_single_child_test_() ->
    Expected = create_leaf([point(1,1),point(2,2),point(3,3)]),
    Leaf1 = create_leaf([point(1,1),point(2,2)]),
    Leaf2 = create_leaf([point(3,3),point(4,4)]),
    ?_assertEqual(Expected, rtree:delete(point(4,4), create_node([Leaf1, Leaf2]))).
    


%% Helper functions
point(X, Y) ->
    {mbr:point_mbr([X, Y]), {X, Y}}.

create_leaf(Content) ->
    node:create_leaf(Content).

create_node(Content) ->
    node:create_node(Content).
