fact(0,1).
fact(N,X) :- N > 0, fact(M,Y), M == N-1, X == N*Y.
