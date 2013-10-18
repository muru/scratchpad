:- use_module(library(clpfd)).

puzzle([S,E,N,D] + [M,O,R,E] = [M,O,N,E,Y]) :-
	Vars = [S,E,N,D,M,O,R,Y]
	, Vars ins 0..9
	, all_distinct(Vars)
	,		  S*1000 + E*100 + N*10 + D +
		 	  M*1000 + O*100 + R*10 + E #=
	M*10000 + O*1000 + N*100 + E*10 + Y,
	M #\= 0, S #\= 0.


%%  N-Queens problem
nqueens([], 0).
nqueens(B, N) :-
	length(B, N)
	, B ins 1..N
	, all_distinct(B)
	, safeboard(B)
	.
safeboard([_]).
safeboard([B1|BS]) :- safe(B1, 1, BS), safeboard(BS).

safe(_, _, []). 
safe(B1, N, [BN|BS]) :- 
	B1 + N #\= BN
	, B1 - N #\= BN
	, N1 is N + 1
	, safe(B1, N1, BS)
	.

%% Knight's Tour (closed).
tour([1], 1).
tour(M, N) :-
	N2 is N*N
	, length(M, N2)
	, M ins 1..N2
	, all_distinct(M)
	, 
	.
