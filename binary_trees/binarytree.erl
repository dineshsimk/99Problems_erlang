-module(binarytree).
-compile(export_all).

%% description of problems are available at
%% https://sites.google.com/site/prologsite/prolog-problems/4

%% binary tree structure
%% Node  -> {node, {Key, Left, Right}}
%% empty -> {node, 'nill'} (or easy will be {} but later)

empty() -> 
	{node, 'nill'}.


%% 4.01 (*) Check whether a given term represents a binary tree
istree({node, {_Key, {node, 'nill'}, {node, 'nill'}}}) -> true;
istree({node, {_Key, Left, Right}}) ->
	istree(Left) and istree(Right);
istree(_) -> false.



%% 4.03 (**) Symmetric binary trees
is_symmetric({node, 'nill'}) -> true;
is_symmetric({node, {_K, L, R}}) -> mirror(L, R).

mirror({node, 'nill'}, {node, 'nill'}) -> true;
mirror({node, {_, LL, LR}}, {node, {_, RL, RR}}) ->
	mirror(LL, LR) andalso mirror(RL, RR);
mirror(_, _) -> false.



%% 4.04 (**) Binary search trees (dictionaries)
construct(List) -> construct(List, {node, 'nill'}).

construct([], Acc) 	  -> Acc;
construct([H|T], Acc) -> construct(T, insert(H, Acc)).



insert(Key, {node, 'nill'}) ->
	{node, {Key, {node, 'nill'}, {node, 'nill'}}};
insert(Key1, {node, {Key2, Left, Right}}) when Key1 < Key2 ->
	{node, {Key2, insert(Key1, Left), Right}};
insert(Key1, {node, {Key2, Left, Right}}) when Key1 > Key2 ->
	{node, {Key2, Left, insert(Key1, Right)}};
insert(Key, {node, {Key, Left, Right}}) ->
	{node, {Key, Left, Right}}.



%% 4.08 (*) Count the leaves of a binary tree
count_leaves({node, 'nill'}) -> 0;
count_leaves({node, {_, {node, 'nill'}, {node, 'nill'}}}) -> 1;
count_leaves({node, {_, L, R}}) -> 
	count_leaves(L) + count_leaves(R).


test_count_leaves() ->
	T_Values = [8,3,10,1,6,14,4,7,13],
	Tree = construct(T_Values),
	case count_leaves(Tree) of
		4 -> test_pass;
		_ -> test_fail
	end.



%% 4.09 (*) Collect the leaves of a binary tree in a list
leaves({node, 'nill'}) -> [];
leaves({node, {K, {node, 'nill'}, {node, 'nill'}}}) -> [K];
leaves({node, {_, L, R}}) ->
	leaves(L) ++ leaves(R).



%% 4.10 (*) Collect the internal nodes of a binary tree in a list
internal({node, 'nill'}) -> [];
internal({node, {_K, {node, 'nill'}, {node, 'nill'}}}) -> [];
internal({node, {K, L, R}}) -> 
	[K] ++ internal(L) ++ internal(R).



%% 4.11 (*) Collect the nodes at a given level in a list
nodes_at({node, 'nill'}, _N) -> [];
nodes_at({node, {K, L, R}}, N) ->
	case N =:= 1 of
		true -> [K];
		_    -> nodes_at(L, N-1) ++ nodes_at(R, N-1)
	end.  





%% HEPER ONES
delete(_Key, {node, 'nill'}) -> {node, 'nill'};
delete(Key, {node, {Key, Left, Right}}) -> merge(Left, Right);
delete(DKey, {node, {Key, Left, Right}}) when DKey < Key ->
	{node, {Key, delete(DKey, Left),Right}};
delete(DKey, {node, {Key, Left, Right}}) when DKey > Key ->
	{node, {Key, Left, delete(DKey, Right)}}.


%% not very efficient need to find something better
merge(Left, {node, 'nill'})  -> Left;
merge({node, 'nill'}, Right) -> Right;
merge({node, {Key, L, R}}, Right) -> 
	merge(merge(L, R), insert(Key, Right)).




no_elems({node, 'nill'})    -> 0;
no_elems({node, {_K, L, R}}) -> 
	1 + no_elems(L) + no_elems(R).


%% binary tree of [8,3,10,1,6,14,4,7,13]
%% http://upload.wikimedia.org/wikipedia/commons/thumb/d/da/Binary_search_tree.svg/200px-Binary_search_tree.svg.png
test_tree() ->
	{node,{8,
       {node,{3,
              {node,{1,{node,nill},{node,nill}}},
              {node,{6,
                     {node,{4,{node,nill},{node,nill}}},
                     {node,{7,{node,nill},{node,nill}}}}}}},
       {node,{10,
              {node,nill},
              {node,{14,
                     {node,{13,{node,nill},{node,nill}}},
                     {node,nill}}}}}}}.

