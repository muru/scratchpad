% Roll No: 133059001
% Name: Murukesh Mohanan
% Date: 18th Oct, 2013

% I, Murukesh Mohanan certify that the I have not indulged in copying
% and all the solution is my own. In case, any discrepancies are
% found, I shall be liable to suitable action.

% You can write any number of predicates you like

% Write the predicates for Problem 1 below:
% Do not change the "name" of the predicate given below
% Your assignment will not be evaluated otherwise

%powerSet(S,Ans):-false.
powerSet([],[[]]).
powerSet([X|Y],S) :- var(S), powerSet(Y,SY), extend(X,SY,SX), append(SY,SX,S).
powerSet(X,S) :- nonvar(S), powerSet(X,SX), equivalent_set(S, SX).

extend(X,[Y], [[X|Y]]).
extend(X,[Y|YS],S) :- extend(X,YS,SX), append([[X|Y]], SX, S).

equivalent_set([],[]).
equivalent_set(X,Y) :- maplist(contains(Y), X), maplist(contains(X), Y).

contains(Y, X) :- is_list(X), member(Z, Y), equivalent_set(Z, X).
contains(Y, X) :- not(is_list(X)), member(X, Y).

is_list([]).
is_list([_|XS]) :- is_list(XS).

% Write the predicates for Problem 2 below:
% Do not change the "name" of the predicate given below
% Your assignment will not be evaluated otherwise

%flatten2(In,Out):-false.

flatten2([],[]).
flatten2([X|Y],Z) :- is_list(X), flatten2(X, L1), flatten2(Y, L2), append(L1, L2, Z).
flatten2([X|Y],[X|L]) :- not(is_list(X)), flatten2(Y, L).

% End of file
