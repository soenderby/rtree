-module(node).
-include_lib("rtree.hrl").
-export([
	 area/1,
	 mbr/1,
	 create_node/1,
	 create_leaf/1,
	 overflow/1,
	 underflow/1,
	 elements/1,
	 create_entry/1
	]).

-spec area(list(rtree:entry(any()))) -> integer().
area(Node) ->
    Mbrs = [Mbr || {Mbr,_} <- Node],
    mbr:area(mbr:mbr(Mbrs)).

%-spec mbr(rtree:entry(any()) | list(rtree:entry(any()))) -> mbr:mbr().
mbr({_, L}) ->
    mbr(L);
mbr(L) ->
    Mbrs = [Mbr || {Mbr, _} <- L],
    mbr:mbr(Mbrs).

%-spec create_node(list(rtree:rtree())) -> rtree:rtree().
create_node(Elems) ->
    {node, [{mbr(E), E} || E <- Elems]}.

-spec create_leaf(list(rtree:entry(any()))) -> rtree:rtree().
create_leaf(Elems) ->
    {leaf, Elems}.

overflow({_Type, Elems}) ->
    overflow(Elems);
overflow(Elems) when is_list(Elems) ->
    length(Elems) > ?MAX.

underflow({_Type, Elems}) ->
    underflow(Elems);
underflow(Elems) when is_list(Elems) ->
    length(Elems) < ?MIN.

-spec elements(rtree:rtree()) -> list(rtree:entry(any())).
elements({_Type, Elems}) ->
    Elems.

-spec create_entry(rtree:rtree()) -> rtree:entry(any()).
create_entry(Node) ->
    {mbr(Node), Node}.
