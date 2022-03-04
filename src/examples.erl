-module(examples).

-export([
	 create_record/0,
	 create_tree/0,
	 delete_record/0
	]).

create_record() ->
    %% records, which are stored in the leaf nodes have the form
    %% {mbr, value}
    %% These can be constructed manually using the mbr module
    %% Manually creating a record for a 2d point:
    Manual_2D_Point = {mbr:point_mbr([2,3]), {2,3}},
    %% The same result can be achieved by using the geo module:
    Geo_2D_Point = geo:point(2,3),
    [Manual_2D_Point, Geo_2D_Point].

create_tree() ->
    %% Insert a record for a point into an empty rtree
    T1 = rtree:insert(geo:point(2,3), rtree:new()),
    %% It is also possible to insert multiple records in a single call
    T2 = rtree:insert([geo:point(3,4), geo:point(9,5), geo:point(11, 14)], T1),
    T2.

delete_record() ->
    %% Using the tree from the previous example
    T1 = create_tree(),
    %% Pass the record to be deleted, and the tree containing it
    T2 = rtree:delete(geo:point(3,4), T1),
    T2.
    
