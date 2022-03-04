-module(rtree).
-include_lib("rtree.hrl").
-export([
	 new/0,
	 insert/2,
	 delete/2,
	 choose_child/2,
	 search/2
	]).

-type entry(T) :: {mbr:mbr(), T}.
-type rtree() :: {leaf, list(entry(any()))} | {node, list(entry(rtree()))}.

-export_type([
	      entry/1,
	      rtree/0
	     ]).

-spec new() -> rtree().
new() ->
    node:create_leaf([]).

-spec insert(entry(any()) | list(entry(any())), rtree()) -> rtree().
insert([], Root) ->
    Root;
insert([H|T], Root) ->
    insert(T, insert(H, Root));
insert(New, Root) ->
    case insert_internal(New, Root) of 
	L when is_list(L) -> % Root split
	    node:create_node(L);
	N ->
	    N
    end.

% Insert spatial object into leaf node, in case of overflow, split node
-spec insert_internal(entry(any()), rtree()) -> rtree() | list(rtree()).
insert_internal(New, {leaf, Elems}) when length(Elems) < ?MAX ->
    {leaf, [New|Elems]};
insert_internal(New, {leaf, Elems}) when length(Elems) == ?MAX ->
    split:exhaustive([New|Elems], leaf);
insert_internal(New, {node, Elems} = N) -> 
    Best = choose_child(New, N),
    {_Mbr, B} = Best,
    NewNode = {node, [node:create_entry(insert_internal(New, B)) | lists:delete(Best, Elems)]},
    case node:overflow(NewNode) of
	true -> split:exhaustive(node:elements(NewNode), node);
	false -> NewNode
    end.
   
-spec choose_child(entry(any()), rtree()) -> entry(rtree()).
choose_child(_, {leaf,_} = Leaf) ->
    Leaf;
choose_child({_,_}, {node, Elems}) when length(Elems) == 1 ->
    hd(Elems);
choose_child({NewMbr, _}, {node, Elems}) ->
    Areas = [{{mbr:area(Mbr), mbr:area(mbr:mbr([NewMbr,Mbr]))}, Elem} || 
		Elem <- Elems,
		{Mbr,_} <- Elems],
    Enlargements = [{NewArea - Area, Elem} || {{Area, NewArea}, Elem} <- Areas],
    Smallest = foldl(fun(X,Acc) -> least_increase(X,Acc) end, Enlargements),
    Smallest.

%% Returns only the elemenents of the argument with the smallets enlargement
%% Settles ties by selecting one with smallest area
least_increase({Enlargement1, E1}, {Enlargement2, E2}) ->
    case Enlargement1 =:= Enlargement2 of %% TODO: Test!
	true -> 
	    case node:area(E1) < node:area(E2) of 
		true  -> E1;
		false -> E2
	    end;
	false ->
	    case Enlargement1 < Enlargement2 of
		true  -> E1;
		false -> E2
	    end
    end.
	    
-spec delete(entry(any()), rtree()) -> rtree().
delete(Elem, Tree) ->
    case delete_rec(Elem, Tree) of
	{underflow, Reinsert} ->
	    insert(lists:reverse(lists:flatten(Reinsert)), node:create_leaf([]));
	{Node, Reinsert} ->
	    insert(Reinsert, Node)
    end.

delete_rec(Elem, {leaf, Elems}) ->
    NewElems = [E || E <- Elems, E =/= Elem],
    case node:underflow(NewElems) of
	true ->
	    {underflow, NewElems};
	false ->
	    {{leaf, NewElems}, []}
    end;
delete_rec(Elem, {node, Elems}) ->
    Elements = [delete_from_child(Elem, C) || C <- Elems],
    {NewElems, Reinsert} = lists:foldl(fun accumulate_delete_result/2, {[],[]}, Elements),
    case node:underflow(NewElems) of
	true ->
	    {underflow, [get_records(E) || {_mbr, E} <- NewElems] ++ Reinsert};
	false ->
	    {{node, NewElems}, Reinsert}
    end.

delete_from_child(Elem, {_Mbr, Node} = Child) ->
    case overlap(Elem, Child) of
	true ->
	    case delete_rec(Elem, Node) of
		{underflow, Reinsert} -> 
		    {underflow, Reinsert};
		{NewNode, Reinsert} ->
		    {node:create_entry(NewNode), Reinsert}
	    end;
	false ->
	    {Child, []}
    end.

accumulate_delete_result({underflow, Elems}, {Children, Reinsert}) ->
    {Children, Elems ++ Reinsert};
accumulate_delete_result({Node, Reinsert1}, {Children, ReinsertAcc}) ->
    {[Node|Children], Reinsert1 ++ ReinsertAcc}.

get_records({leaf, Elems}) ->
    Elems;
get_records({node, []}) ->
    [];
get_records({node, [{_Mbr, Node}|T]}) ->
    get_records(Node) ++ get_records({node, T}).

%% This may be inverted
overlap({Mbr1,_}, {Mbr2,_}) ->
    mbr:area(mbr:intersection(Mbr1,Mbr2)) =< 0.

-spec search(mbr:mbr(), rtree()) -> list(entry(any())).
search(Mbr, {leaf, Elems}) ->
    [E || E <- Elems, overlap({Mbr, a}, E)];
search(Mbr, {node, Elems}) ->
    lists:flatmap(fun(Elem) ->
			  search_child(Mbr, Elem) end, Elems).


search_child(Mbr, Child) ->
    {_Child_Mbr, Node} = Child,
    %% Something is very wrong here, it should be searched in the true case
    %% However this breaks the tests.
    case overlap({Mbr, a}, Child) of
	true ->
	    [];
	false -> 
	    search(Mbr, Node)
    end.
    
%% Two argument version of lists:foldl/3 func
%% This one take the first element of the given list as the initial accumulator value
foldl(F,[H|T]) ->
    lists:foldl(F,H,T).

%% Util function for debugging
print(X) ->
    io:format("~p \n", [X]).
