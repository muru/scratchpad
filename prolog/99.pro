len([],0).
len([_],1).
len([H|T], X) :- len(T,Y), X is Y+1.

my_last(X,[X]).
my_last(X,[H|T]) :- my_last(X,T).

penultimate(X, [X,_]).
penultimate(X, [H|T]) :- penultimate(X, T).

element_at(_,[],_) :- false.
element_at(X,[X|T],N) :- len ([X|T], N).
element_at(X,[H|T],N) :- len([H|T], L), L >= N, M is N-1, element_at(X,T,M).
