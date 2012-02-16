-module(lists_1_10).
-compile(export_all).

%% 1.01 - Find the last element of a list.
last([H|[]]) -> H;
last([_|T])  -> last(T).



%% 1.02 - Find the last but one element of a list.
second_last([Sl, _|[]]) ->
	Sl;
second_last([_|L]) ->
	second_last(L).



%% 1.03 - Find the K'th element of a list.
findkth(L, N) -> findkth(L, N, 1).

findkth([H|_], N, N) -> H;
findkth([_|T], N, Tmp) ->
	findkth(T, N, Tmp + 1).

% alternative
find_kth([H|_], 1) -> H;
find_kth([_|T], N) ->
	find_kth(T, N-1).



%% 1.04 - Find the number of elements of a list.
list_length(L) -> list_length(L, 0).

list_length([], Acc) -> Acc;
list_length([_|T], Acc) ->
	list_length(T, Acc + 1).



%% 1.05 - Reverse a list.
list_reverse([]) -> [];
list_reverse([H|T]) ->
	list_reverse(T) ++ [H].

% Using tail recursion.
list_reverse_tail(L) -> list_reverse_tail(L, []).
list_reverse_tail([], Acc) -> Acc;
list_reverse_tail([H|T], Acc) ->
	list_reverse_tail(T, [H|Acc]).



%% 1.06 - Find out whether a list is a palindrome.
palindrome([]) -> true;
palindrome(L)  -> L =:= list_reverse_tail(L).



%% 1.07 - Flatten a nested list structure.
flatten([])    -> [];
flatten([H|T]) -> flatten(H) ++ flatten(T);
flatten(Value) -> [Value].



%% 1.08 - Eliminate consecutive duplicates of list elements.
dups_remove([]) 		-> [];
dups_remove([H,H|L]) 	-> dups_remove([H|L]);
dups_remove([H|L]) 		-> [H | dups_remove(L)].
%dups_remove([H|L]) 	-> [H] ++ dups_remove(L).


%% 1.09 - Pack consecutive duplicates of list elements into sublists.
compress([]) 	-> [];
compress([H|T]) -> dups_sublist(H, [H], T).

dups_sublist(H, Hs, [H|T]) -> dups_sublist(H, [H|Hs], T);
dups_sublist(_, Hs, T) 	   -> [Hs | compress(T)].



%% 1.10 - Run-length encoding of a list.
encode([]) -> [];
encode(List) ->
	[[list_length(X), hd(X)] || X <- compress(List)].
