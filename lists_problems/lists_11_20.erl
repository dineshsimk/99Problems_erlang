-module(lists_11_20).
-import(lists_1_10, [compress/1, list_length/1, encode/1, flatten/1]).
-compile(export_all).


%% 1.11 - Modified run-length encoding.
encode_modified([]) -> [];
encode_modified(List) ->
	[encode_helper(X) || X <- compress(List)].

encode_helper(X) ->
	case X of
	  [H|[]] -> H;
	  _ 	   -> [list_length(X), hd(X)]
	end.

% Anternatives
encode_modified_2(List) ->
	[case L of 1 -> E; _ -> [L, E] end || [L, E] <- encode(List)].

encode_modified_3(List) ->
	[if L > 1 -> [L, E]; true -> E end || [L, E] <- encode(List)].



%% 1.12 - Decode a run-length encoded list.
decode([]) -> [];
decode(List) ->
	flatten([decode_helper(X) || X <- List]).

decode_helper(X) ->
	case X of
		[Len, E] -> lists:duplicate(Len, E);
		_		 -> X
	end. 

% alternative
decode_2([]) -> [];
decode_2([H|T]) when is_list(H) -> 
	[Len, E] = H, lists:duplicate(Len, E) ++ decode_2(T);
decode_2([H|T]) -> [H | decode_2(T)].



%% 1.14 - Duplicate the elements of a list.
dupli([]) -> [];
dupli([H|T]) -> [H,H | dupli(T)].



%% 1.15 - Duplicate the elements of a list a given number of times.
dupli_num([], _) -> [];
dupli_num([H|T], N) -> lists:duplicate(N, H) ++ dupli_num(T, N).

% alternatives
% dupli_num_2(List, N) -> flatten(lists:duplicate(N, List)).
% nop,  wrong logic


%% 1.16 - Drop every N'th element from a list.

drop(List, N) -> lists:reverse(drop(List, N, 1, [])).

drop([], _, _, Acc) -> Acc;
drop([H|T], N, Count, Acc) -> 
	case Count rem N == 0 of
		true  -> drop(T, N, Count + 1, Acc);
		_	  -> drop(T, N, Count + 1, [H |Acc])
	end.

% or
drop_2(List, N) -> drop_2(List, N, 1, []).

drop_2([],    _,     _, Acc) -> Acc;
drop_2([_|T], N,     N, Acc) -> drop_2(T, N, 1, Acc);
drop_2([H|T], N, Count, Acc) -> drop_2(T, N, Count + 1, Acc ++ [H]).



%% 1.17 - Split a list into two parts; the length of the first part is given.
split(N, List) -> 
	case N > length(List) of
		true  -> erlang:error(invalid_arguments);
		false -> split(N, List, 0, [])
	end.

split(_, [], _, Acc) -> {Acc, []};
split(N, Ls, N, Acc) -> {Acc, Ls};
split(N, [H|T], Count, Acc) ->
	split(N, T, Count+1, Acc ++ [H]).



%% 1.18 - Extract a slice from a list.
slice(List, N1, N2) -> 
	case length(List) < N1 orelse N2 > length(List) of
		true  -> erlang:error(invalid_arguments);
		false -> slice(List, N1, N2, 1, [])
	end.

slice([], _, _, _, Acc) -> Acc;
slice([H|_], _, N2, N2, Acc) -> Acc ++ [H];
slice([H|T], N1, N2, Count, Acc) when Count >= N1, Count =< N2 ->
	slice(T, N1, N2, Count+1, Acc ++ [H]);
slice([_|T], N1, N2, Count, Acc) ->
	slice(T, N1, N2, Count+1, Acc).


% alternative, easy to read/good version
slice_2(L, S, F) -> lists:reverse(slice_2(L, S, F, [])).

slice_2(_, 1, 0, Acc) -> Acc;
slice_2([H|T], 1, E, Acc) -> slice_2(T, 1, E-1, [H|Acc]);
slice_2([_|T], S, E, Acc) -> slice_2(T, S-1, E-1, Acc).



%% 1.19 - Rotate a list N places to the left.
rotate(L, N) when N > 0 -> 
	{Lf, Rt} = split(N, L), 
	Rt ++ Lf;
rotate(L, N) ->
	{Lf, Rt} = split(length(L)+N, L),
	Rt ++ Lf.


%% 1.20 - Remove the K'th element from a list.
remove_at(List, N) -> remove_at(List, N, 1, []).

remove_at([_|T], N, N, Acc) -> Acc++T;
remove_at([H|T], N, C, Acc) -> remove_at(T, N, C+1, Acc++[H]).

% non-tail recursion
remove_at_2([_|T], 1) -> T;
remove_at_2([H|T], N) -> [H|remove_at_2(T, N-1)].
