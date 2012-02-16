-module(arithmatic).
-compile(export_all).


%% solution to the arithmatic problems available at following page
%% https://sites.google.com/site/prologsite/prolog-problems/2
%% or see arth_problems.txt for problem details.

%% 2.01 (**) Determine whether a given integer number is prime.
is_prime(N) -> is_prime(N, trunc(math:sqrt(N)), true).

is_prime( 2, _C, _Acc) -> true;
is_prime(_N,  1,  Acc) -> Acc;
is_prime( N,  C, _Acc) -> 
	%% bit weired logic here, if number get divided by
	%% C immediately return false, otherwise keep on looping
	%% untill C == 1, if value is never got false till count 1 then 
	%% its prime number
	case N rem C =:= 0 of
		true  -> is_prime(N,   1, false);
		false -> is_prime(N, C-1,  true)
	end.


%% alternative,
%% more efficient then first one
is_prime2(N) -> is_prime2(N,2).

is_prime2(N, V) ->
	C = trunc(math:sqrt(N)),
	case V > C of
		true  -> true;
		false -> case N rem V of
					0 -> false;
					_ -> is_prime2(N, V+1)
				  end
	end. 
	

test_is_prime() ->
	Prime_array = [2,3,5,7,11,13,23,31,37,41,53,67,71,4,6,9,21,27,45,50,66,99],
	[{X, is_prime(X)} || X <- Prime_array].



%% extra problem %%
%% get next prime, start searching from the given number
%% till the prime number is foundg
next_prime(N) -> next_prime(N, false).

next_prime(N, true) -> N;
next_prime(N,    _) -> next_prime(N+1, is_prime(N+1)).



%% 2.02 (**) Determine the prime factors of a given positive integer.
prime_factors(N) -> lists:reverse(prime_factors(N, next_prime(1), [])).

prime_factors(1, _,  Acc) -> Acc;
prime_factors(N, Np, Acc) ->
	case N rem Np =:= 0 of
		true  -> prime_factors(N div Np, next_prime(1), [Np | Acc]);
		false -> prime_factors(N, next_prime(Np), Acc)
	end.




%% 2.03 (**) Determine the prime factors of a given positive integer (2).
prime_factors_mult(N) -> 
	Pg = lists:sort(prime_factors(N)),
	[[hd(X), length(X)] || X <- prime_group(Pg)].
	

%% groups the matching consucative element into a sub-list.
prime_group([]) -> [];
prime_group([H|T]) -> prime_group(H, [H], T).

prime_group(H, Hs, [H|T]) -> prime_group(H, [H|Hs], T);
prime_group(_, Hs, T)     -> [Hs | prime_group(T)].



%% 2.04 (*) A list of prime numbers.
%% Given a range of integers by its lower and upper limit, 
%% construct a list of all prime numbers in that range.
prime_list(L, U) -> lists:reverse(prime_list(L, U+1, [])).

prime_list(U, U, Acc) -> Acc;
prime_list(L, U, Acc) -> 
	case is_prime(L) of
		true -> prime_list(L+1, U, [L|Acc]);
		_    -> prime_list(L+1, U, Acc)
	end. 



%% 2.05 (**) Goldbach's conjecture.
goldbach(N) -> 
	case N rem 2 =:= 0 of
		true  -> Pb = pb(N-2), 
				 [N-Pb, Pb];
		false -> [N]
	end.

pb(1) -> 0;
pb(N) -> 
	case is_prime(N) of
		true  -> N;
		false -> pb(N-1)
	end.


%% 2.06 (**) A list of Goldbach compositions.
goldbach_list(L, U) -> lists:reverse(goldbach_list(L, U+1, [])).

goldbach_list(U, U, Acc) -> Acc;
goldbach_list(L, U, Acc) when L rem 2 =:= 0 ->
	goldbach_list(L+1, U, [{L, goldbach(L)}|Acc]);
goldbach_list(L, U, Acc) -> goldbach_list(L+1, U, Acc).



%% 2.07 (**) Determine the greatest common divisor of 
%% two positive integer numbers.
gcd(N1, N2) -> gcd(N1, N2, N1 div 2).

gcd(_N1, _N2, 1) -> 1;
gcd(N1, N2, Cd) when N1 rem Cd =:= 0 ->
	case N2 rem Cd =:= 0 of
		true -> Cd;
		_    -> gcd(N1, N2, Cd-1)
	end;
gcd(N1, N2, Cd) -> gcd(N1, N2, Cd-1).



%% 2.08 (*) Determine whether two positive integer numbers are coprime.
coprime(N1, N2) ->
	case gcd(N1, N2) of
		1 -> true;
		_ -> false
	end.



%% 2.09 (**) Calculate Euler's totient function phi(m).

%% returnes the length of co-primes of N,
t_prime(N) -> length(t_prime(N, N-1, [])).

%% returnes the actual co-primes of N,
t_primes(N) -> t_prime(N, N-1, []).

%% finds co-primes with N
t_prime(_N, 0, Acc) -> Acc;
t_prime(N, C, Acc) ->
	case  coprime(N, C) of
		true  -> t_prime(N, C-1, [C|Acc]);
		false -> t_prime(N, C-1, Acc)
	end.


%% 2.10 (**) Calculate Euler's totient function phi(m) (2).
phi_2(N) -> 
	phi_2(prime_factors_mult(N), 1).

phi_2([], Acc) -> Acc;
phi_2([[P, M]|T], Acc) -> phi_2(T, (P-1)*(math:pow(P, M-1))*Acc).



%% 2.11 (*) Compare the two methods of calculating 
%% Euler's totient function.

%% to test give some large input like N=10009
efficiency(N) ->
	statistics(wall_clock),
	Et1 = t_prime(N),
	%% statistics(wall_clock) returns the following tuple:
	%% {Total_Wallclock_Time, Wallclock_Time_Since_Last_Call}.
	{_, T1} = statistics(wall_clock),
	io:format("Time taken by t_prime(N) function is ~p~n", [(T1/1000)]),

	statistics(wall_clock),
	Et2 = t_prime(N),
	{_, T2} = statistics(wall_clock),
	io:format("Time taken by phi_2(N) function is ~p~n", [(T2/1000)]),

	case (Et1 =/= Et2) of
		true -> 
			io:format("Some thig wrong in one of the algorithm");
		_    -> 
			case T1 > T2 of
				true -> io:format("phi_2(N) is faster by ~p sec~n", [(T1-T2)/1000]);
				_    -> io:format("t_prime(N) is faster by ~p sec~n", [(T2-T1)/1000])
				end
	end.

	

