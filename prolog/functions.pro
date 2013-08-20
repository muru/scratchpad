fact(0,1).
fact(N,X) :- M == N-1, fact(M,Y), X == N*Y.
