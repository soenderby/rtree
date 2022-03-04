-module(insert_tests).
-include_lib("eunit/include/eunit.hrl").

create_leaf_test_() ->
    [
     ?_assertEqual(
	{leaf, [point(1,1), point(2,2)]}, 
	create_leaf([point(1,1), point(2,2)]))
    ].

create_node_test_() ->
    Leaf1 = create_leaf([point(1,1), point(2,2)]),
    Leaf2 = create_leaf([point(8,8), point(9,9)]),
    Expected = {node, [{node:mbr(Leaf1), Leaf1},{node:mbr(Leaf2), Leaf2}]},
    ?_assertEqual(Expected, create_node([Leaf1, Leaf2])).

insert_leaf_test_() ->
    [
     ?_assertEqual(
	create_leaf([point(1,2)]),
	rtree:insert(point(1,2), create_leaf([]))),
     ?_assertEqual(
	create_leaf([point(1,2), point(3,4)]),
	rtree:insert(point(1,2), create_leaf([point(3,4)]))),
     ?_assertEqual(create_leaf([point(3,3)]), rtree:insert(point(3,3), rtree:new()))
    ].

insert_split_root_test_() ->
    InitialElems = [point(2,2), point(8,8), point(9,9)],
    Root = create_leaf(InitialElems),
    Leaf1 = create_leaf([point(8,8), point(9,9)]),
    Leaf2 = create_leaf([point(1,1), point(2,2)]),
    
    ?_assertMatch(
       {node, [{_, Leaf1}, {_, Leaf2}]},
       rtree:insert(point(1,1), Root)
      ).

choose_child_test_() ->
    [
     % Given leaf, returns leaf
     ?_assertEqual(create_leaf(any), rtree:choose_child(elem, create_leaf(any))),
     % Given tree with single child, returns child
     ?_assertEqual(create_leaf(any), rtree:choose_child(point(1,1), rtree([create_leaf(any)]))),
     % Given spatial object and node, should select child requiring least enlargement
     ?_assertEqual({mbr:point_mbr([2,2]), create_leaf(two)}, 
		   rtree:choose_child(point(1,1), rtree([
     							 {mbr:point_mbr([5,5]), create_leaf(one)},
							 {mbr:point_mbr([2,2]), create_leaf(two)}
							])))
    ].

insert_internal_node_test_() ->
    Create_Leaf1 = create_leaf([point(8,8), point(9,9)]),
    Create_Leaf2 = create_leaf([point(1,1), point(2,2)]),
    ExpectedCreate_Leaf2 = create_leaf([point(3,3), point(1,1), point(2,2)]),
    InitialRoot = create_node([Create_Leaf1, Create_Leaf2]),
    Expected = create_node([ExpectedCreate_Leaf2, Create_Leaf1]),
    ?_assertEqual(
       Expected,
       rtree:insert(point(3,3), InitialRoot)
      ).
%% Helper function
point(X, Y) ->
    {mbr:point_mbr([X, Y]), {X, Y}}.

rtree(Content) ->
    {node, Content}.

create_leaf(Content) ->
    node:create_leaf(Content).

create_node(Content) ->
    node:create_node(Content).
