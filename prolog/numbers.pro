lt(0,_).
lt(g(X),g(Y)) :- lt(X,Y).

eq(X,Y) :- lt(X,Y), lt(Y,X). 

add(0,X,X).
add(g(X),Y,g(Z)) :- add(X,Y,Z).

sub(X,Y,Z) :- add(Y,Z,X).

mult(0,_,0).
mult(g(X),Y,Z) :- mult(X,Y,Z1), add(Y,Z1,Z).

len(nl,0).
len(l(H,T), Z): len(T,Z1), add(g(0),Z1,Z).
