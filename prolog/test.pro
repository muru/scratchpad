enum(0).
enum(N) :- enum(M), N is M + 1.
enumfrom(A,B,N):- enum(N), N>=A,B>=N.
