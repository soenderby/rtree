
# Table of Contents

1.  [R-tree datastucture implementation](#org40a068d)
    1.  [Basic usage](#org44aeac0)
        1.  [Creating records](#org0a46b93)
        2.  [Creating trees](#org755758b)
        3.  [Deleting records](#orge674c58)
        4.  [Searching trees](#orgaf3d609)
    2.  [Reference](#orgbcc1a43)
        1.  [Modules](#orgaeeb86a)



<a id="org40a068d"></a>

# R-tree datastucture implementation

The R-tree datastructure and associated algorithms was introduced in [Guttman1984](https://dl.acm.org/doi/abs/10.1145/602259.602266).
The goal of this project is to implement this datastructure in Erlang.


<a id="org44aeac0"></a>

## Basic usage

This section contains examples and explaination of the basic usage of the library.


<a id="org0a46b93"></a>

### Creating records

Values are stored in the leaf node records of an rtree.
These records have the form *{mbr, value}*.
These can be constructed manually using the *mbr* module.

The following example shows how to create a record for a two dimensional point.

    Manual_2D_Point = {mbr:point_mbr([2,3]), {2,3}}.

As points are quite useful when dealing with geometry, functionality for creating records for points is provided by the *geo* module. The following example shows creating the same record from the previous example using the *geo* module.

    Geo_2D_Point = geo:point(2,3).


<a id="org755758b"></a>

### Creating trees

A new empty tree can be created using the *new/0* function. 
Records can be inserted into an existing tree using the *insert/2* function.

The following example shows a function that creates a new tree and inserts multiple records.

    create_tree() ->
        %% Insert a record for a point into an empty rtree
        T1 = rtree:insert(geo:point(2,3), rtree:new()),
        %% It is also possible to insert multiple records in a single call
        T2 = rtree:insert([geo:point(3,4), geo:point(9,5), geo:point(11, 14)], T1),
        T2.


<a id="orge674c58"></a>

### Deleting records

Records can be deleted from a tree using the *delete/2* function.
The function requires the exact record to be deleted to be specified.

The following example shows deleting a record from the tree created in the previous example.

    delete_record() ->
        %% Using the tree from the previous example
        T1 = create_tree(),
        %% Pass the record to be deleted, and the tree containing it
        T2 = rtree:delete(geo:point(3,4), T1),
        T2.


<a id="orgaf3d609"></a>

### Searching trees


<a id="orgbcc1a43"></a>

## Reference


<a id="orgaeeb86a"></a>

### Modules

The project is made of four modules.

-   rtree.erl: Exposes functions for manipulating rtrees.
-   node.erl: Provides operations on tree nodes.
-   mbr.erl: Contains functions for creating and operating on minimum bounding rectangles.
-   split.erl: Contains the algorithm(s) used to split nodes in the case of an overflow

1.  rtree.erl

    Exports functions
    
    -   new() -> rtree().
    -   insert(entry(any()) | list(entry(any())), rtree()) -> rtree().
    -   delete(entry(any()), rtree()) -> rtree().
    -   search(mbr:mbr(), rtree()) -> list(entry(any())).
    
    Exports types
    
    -   entry/1
    -   rtree/0
    
    The new function create a new rtree, consisting of an empty leaf.
    
    The insert and delete functions accept a record and an rtree and return a modified rtree. In the case of insert, it also accepts a list of records to insert.
    The search functions accepts a search rectangle and an rtree, and returns a list of the records that overlap the search rectangle.

2.  node.erl

    Exports functions
    
    -   area/1,
    -   mbr/1
    -   create_node/1
    -   create_leaf/1
    -   overflow/1
    -   underflow/1
    -   elements/1
    -   create_entry/1
    
    The functions in the node.erl module create and operate on the data structures used, so as to provide an abstraction that hides the implementation of the structures.

3.  mbr.erl

    Exports functions
    
    -   point_mbr(list(integer())) -> mbr().
    -   mbr(list(mbr())) -> mbr().
    -   area(mbr()) -> integer().
    -   intersection(mbr(), mbr()) -> mbr().
    
    Exports types
    
    -   mbr/0.
    
    The mbr.erl module provides functions to create minimum bounding rectangles and perform calulations on these.
    
    A minimum bounding rectangle is represented by a list, with elements being min and max values for a single dimension.
    A point has a simple mbr, as the min and max values have the same values for all the dimensions. Such an mbr can be created using the point_mbr/1 function.
    The combined mbr for multiple other mbrs, each one describing an object, can be calculated using the mbr/1 function.
    Using point_mbr/1 and mbr/1, it is possible to create the mbr for any geometry that can be described by points.

4.  split.erl

    Exports functions
    
    -   exhaustive(list(rtree:rtree()), atom()) -> list(rtree:rtree()).
    
    The split.erl module defines the algorithms for splitting nodes in case of overflow.
    Although there are multiple possible algorithms, the module currently only exports a single one. Any future node splitting algorithms should be added to this module.

