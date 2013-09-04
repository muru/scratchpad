mother(m, c).
father(f, c).
male(x).
female(y).

grandmother(G, X) :- female(M), mother(G, M), mother(M, X).
grandmother(G, X) :- male(F), mother(G, F), father(F, X).

sister(S, X) :- female(S), male(F), father(F, X), father(F, S), X \== S.
sister(S, X) :- female(S), female(M), mother(M, X), mother(M, S), X \== S.

elem(_, []) :- false.
elem(X, [X|_]).
elem(X, [_|T]) :- elem(X, T).

twice(X, [X|T]) :- elem(X, T).
twice(X, [_|T]) :- twice(X, T).

binvalue([], 0).
binvalue([1|T], X) :- length(T, L), Y is 2**L, binvalue(T, X1), X is X1 + Y.
binvalue([0|T], X) :- binvalue(T, X).

enum(0).
enum(X) :- enum(X1), X is X1 + 1.
fact(0, 1).
fact(X, Y) :- nonvar(X), X > 0, X1 is X - 1, fact(X1, Y1), Y is X*Y1.
fact(X, Y) :- var(X), Y >= 1, enum(X1), fact(X1, Y), X is X1.
