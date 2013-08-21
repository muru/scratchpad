len([],0).
len([_],1).
len([H|T], X) :- X

my_last(X,[X]).
my_last(X,[H|T]) :- my_last(X,T).

penultimate(X, [X,_]).
penultimate(X, [H|T]) :- penultimate(X, T).
