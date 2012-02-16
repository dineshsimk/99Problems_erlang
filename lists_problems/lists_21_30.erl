-module(lists_21_30).
-import(lists_1_10, [encode/1]).
-import(lists_11_20, [split/2, remove_at/2]).
-compile(export_all).


%% 1.21 - Insert an element at a given position into a list.
insert_at(El, List, P) ->
	{L, R} = split(P-1, List),
	L ++ [El] ++ R.



%% 1.22 - Create a list containing all integers within a given range.
range(N1, N2) -> lists:reverse(range(N1, N2, [])).

range(N2, N2, Acc) -> [N2] ++ Acc;
range(N1, N2, Acc) -> range(N1+1, N2, [N1|Acc]).

%alternative
range_2(N2, N2) -> [N2];
range_2(N1, N2) -> [N1 | range_2(N1+1, N2)].



%% 1.23 - Extract a given number of randomly selected elements from a list.
md_select(List, N) -> md_select(List, N, []).

md_select(_, 0, Acc) -> Acc;
md_select(List, N, Acc) -> 
	Index = random:uniform(length(List)),
	md_select(remove_at(List, Index), N-1, Acc ++ [value_at(Index, List)]).

value_at(1, [H|_]) -> H;
value_at(N, [_|T]) -> value_at(N-1, T).



%% 1.24 - Lotto: Draw N different random numbers from the set 1..M.
rnd_select(N, R) ->
	md_select(range(1, R), N).




%% 1.25 - Generate a random permutation of the elements of a list.
rnd_permu(List) -> md_select(List, length(List)).




%% 1.28 (**) Sorting a list of lists according to length of sublists
%% sorted using quick short, not effective pivot selection though

%% a. sort by list length
lsort([]) -> [];
lsort([H|T]) -> 
	lsort([Smaller || Smaller <- T, length(Smaller) =< length(H)])
		++ [H] ++
	lsort([Larger || Larger <- T, length(Larger) > length(H)]).


%% b. sort by length frequency
lfsort([]) -> [];
lfsort(Lists) ->
	SortedByLen = lists:sort(len_tuple(Lists)),
	Freq = lists:keysort(2, lists:map(fun([X,Y]) -> {Y, X} end, encode(SortedByLen))),
	[create(Ll, Lists) || {Ll, _} <- Freq].

create(L, Lists) ->
	[X || X <- Lists, length(X) == L].



len_tuple(Lists) ->
	[length(X) || X <- Lists].