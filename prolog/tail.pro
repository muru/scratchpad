rv([], []).
rv([X|L1], L2) :- rv(L1,LR), append(LR, [X], L2).

rev(L1, L2) :- reva(L1, [], L2).
reva([X|XS], R, L):- reva(XS, [X|R], L).
reva([], L, L).

isPalindrome(L) :- rev(L,L).

ctr(0).
ctr(N) :- ctr(N1), N is N1+1.

%ctr(M,N,X) :- ctr(X), X >= M, (N =< X -> !; true).
ctr(M, N, M) :- M < N.
ctr(M, N, X) :- M1 is M + 1, M1 < N, ctr(M1, N, X).

allp(N,[X,Y]) :- ctr(0, N, Y), ctr(0, N, X).

split(N, 1, [N]).
split(N, M, [X|L]) :- ctr(1, N, X), M1 is M - 1, N1 is N - X, split(N1, M1, L). 

count(X, L, N) :- counta(X,L,N,0).
counta(_, [], N, N).
counta(X, [X|XS], N, M) :- M1 is M + 1, counta(X, XS, N, M1).
counta(X, [Y|XS], N, M) :- Y \= X, counta(X, XS, N, M).

ssq(L, X, N) :- ssqa(X, L, N, 0).
ssqa([], _, N, M) :- N is M + 1.
ssqa([_|_], [], N, N).
ssqa([X|XS], [X|L], N, M) :- ssqa(XS, L, M1, M), ssqa([X|XS], L, N, M1).
ssqa([X|XS], [Y|L], N, M) :- Y \= X, ssqa([X|XS], L, N, M).


