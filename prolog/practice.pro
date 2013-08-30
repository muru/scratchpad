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



