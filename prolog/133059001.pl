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
powerSet(X,S) :- nonvar(S), maplist(subsetof(X), S).

extend(X,[Y], [[X|Y]]).
extend(X,[Y|YS],S) :- extend(X,YS,SX), append([[X|Y]], SX, S).

subsetof(_,[]).
subsetof(S,[X|XS]) :- member(X,S), subsetof(S,XS).

% Write the predicates for Problem 2 below:
% Do not change the "name" of the predicate given below
% Your assignment will not be evaluated otherwise

%flatten(In,Out):-false.
flatten([],[]).







% End of file
