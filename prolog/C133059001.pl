%%
% Name: Murukesh Mohanan
% Roll no.: 133059001
% Written all this code myself, I have. Cheated, I have not.
% (That was Yoda-speak.)
%
elem_num([], []).
elem_num([X|XS], [(X, 1)|L]) :- not(member(X, XS)), elem_num(XS, L).
elem_num([X|XS], [(X, N)|L]) :- 
	member(X, XS) 
	, elem_num(XS, L1)
	, select((X, N1), L1, L)
	, N is N1 + 1.

crdnlty(M, N) :- once(elem_num(M, L)), length(L, N).

crdnltyLess(K, (_, N)) :- N =< K.
crdnltyLow(M, K, N) :- 
	once(elem_num(M, L))
	, include(crdnltyLess(K), L, LS)
	, length(LS, N)
	.

crdnltys([], []).
crdnltys([(_,N)|L], [N|NS]) :- crdnltys(L,NS).

crdnltyEq(K, (_, K)).
crdnltyMin([],0,0).
crdnltyMin(M, N1, N2) :- 
	once(elem_num(M, L))
	, crdnltys(L, K)
	, min_list(K, N2)
	, include(crdnltyEq(N2), L, LS)
	, length(LS, N1)
	.
